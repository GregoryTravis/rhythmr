#!/usr/bin/ruby
$: << "#{ENV['HOME']}/.../lib/ruby"

require 'util'

# Too hard-coded, of course
assert ARGV.length == 1
app = ARGV[0]
exe = "Rhythmr/#{app}/Contents/MacOS/rhythmr"
frameworks = "Rhythmr/#{app}/Contents/Frameworks"

def basename(p)
  `basename #{p}`.chomp
end

deps = {
  "/usr/local/opt/libsndfile/lib/libsndfile.1.dylib" => [
          "/usr/local/opt/libsndfile/lib/libsndfile.1.dylib",
          "/usr/local/opt/flac/lib/libFLAC.8.dylib",
          "/usr/local/opt/libogg/lib/libogg.0.dylib",
          "/usr/local/opt/libvorbis/lib/libvorbis.0.dylib",
          "/usr/local/opt/libvorbis/lib/libvorbisenc.2.dylib",
  ],
  "/usr/local/opt/flac/lib/libFLAC.8.dylib" => [
          "/usr/local/opt/flac/lib/libFLAC.8.dylib",
          "/usr/local/opt/libogg/lib/libogg.0.dylib",
  ],
  "/usr/local/opt/libogg/lib/libogg.0.dylib" => [
          "/usr/local/opt/libogg/lib/libogg.0.dylib",
  ],
  "/usr/local/opt/libvorbis/lib/libvorbis.0.dylib" => [
          "/usr/local/opt/libvorbis/lib/libvorbis.0.dylib",
          "/usr/local/opt/libogg/lib/libogg.0.dylib",
  ],
  "/usr/local/opt/libvorbis/lib/libvorbisenc.2.dylib" => [
          "/usr/local/opt/libvorbis/lib/libvorbisenc.2.dylib",
          # otool original reported this symlink-less path but changed it back
          # for consistency
          #/usr/local/Cellar/libvorbis/1.3.6/lib/libvorbis.0.dylib
          "/usr/local/opt/libvorbis/lib/libvorbis.0.dylib",
          "/usr/local/opt/libogg/lib/libogg.0.dylib",
  ],
  "/usr/local/opt/portaudio/lib/libportaudio.2.dylib" => [
          "/usr/local/opt/portaudio/lib/libportaudio.2.dylib",
  ]
}

# # In one case the unsymlinked path is used, so we revert it
# def resymlink(path)
#   h = { "/usr/local/Cellar/libvorbis/1.3.6/lib/libvorbis.0.dylib" => "/usr/local/opt/libvorbis/lib/libvorbis.0.dylib" }
#   if h.include?(path)
#     h[path]
#   else
#     path
#   end
# end

puts deps

# Transitive closure
referring = deps.keys.sort
referenced = deps.values.flatten(1).uniq.sort
assert referring == referenced

libs = referring

abs_to_rel = Hash[libs.map { |lib| [lib, "#{frameworks}/#{basename(lib)}"] }]
abs_to_dylib_path = Hash[libs.map { |lib| [lib, "@loader_path/../Frameworks/#{basename(lib)}"] }]

# Localize exe
libs.map { |lib|
  #puts lib
  cmd("cp #{lib} #{abs_to_rel[lib]}")
  cmd("install_name_tool -change #{lib} #{abs_to_dylib_path[lib]} #{exe}")
}

# Localize libs
deps.map { |lib, libdeps|
  #cmd("install_name_tool -change #{abs_to_rel[lib]} #{abs_to_dylib_path[lib]} #{exe}")
  #cmd("install_name_tool -change #{lib} #{abs_to_dylib_path[lib]} #{abs_to_rel[lib]}")
  libdeps.map { |libdep|
    cmd("install_name_tool -change #{libdep} #{abs_to_dylib_path[libdep]} #{abs_to_rel[lib]}")
  }
}

# Just one more, because one of the deps doesn't use the symlink like the others
cmd("install_name_tool -change /usr/local/Cellar/libvorbis/1.3.6/lib/libvorbis.0.dylib @loader_path/../Frameworks/libvorbis.0.dylib Rhythmr/Rhythmr.app/Contents/Frameworks/libvorbisenc.2.dylib")

# Show results
rels = abs_to_rel.values.flatten(1).uniq.map { |relpath|
  cmd("otool -L #{relpath}")
}

    # echo localizing $dylib
    # cp $dylib Rhythmr/$app/Contents/Frameworks
    # install_name_tool -change $dylib @loader_path/../Frameworks/$(basename $dylib) Rhythmr/$app/Contents/MacOS/rhythmr
