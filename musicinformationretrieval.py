import sys

# From https://musicinformationretrieval.com/hpss.html

print(sys.path)
sys.path.insert(0, "/usr/local/lib/python2.7/site-packages")
#%matplotlib inline
import seaborn
import numpy, scipy, matplotlib.pyplot as plt # , IPython.display as ipd
import librosa, librosa.display
plt.rcParams['figure.figsize'] = (14, 5)
xh, sr_h = librosa.load('prelude_cmaj.wav', duration=7, sr=None)
xp, sr_p = librosa.load('125_bounce.wav', duration=7, sr=None)
print len(xh), len(xp)
print sr_h, sr_p
x = xh/xh.max() + xp/xp.max()
x, sr_p = librosa.load('Grace Jones - Pull Up To The Bumper-Tc1IphRx1pk.wav', duration=45, sr=None)
x = 0.5 * x/x.max()
x.max()
X = librosa.stft(x)
Xmag = librosa.amplitude_to_db(X)
librosa.display.specshow(Xmag, sr=sr_h, x_axis='time', y_axis='log')
plt.colorbar()
H, P = librosa.decompose.hpss(X)
Hmag = librosa.amplitude_to_db(H)
Pmag = librosa.amplitude_to_db(P)
librosa.display.specshow(Hmag, sr=sr_h, x_axis='time', y_axis='log')
plt.colorbar()
librosa.display.specshow(Pmag, sr=sr_p, x_axis='time', y_axis='log')
plt.colorbar()
h = librosa.istft(H)
p = librosa.istft(P)
librosa.output.write_wav("outh.wav", h, 44100)
librosa.output.write_wav("outp.wav", p, 44100)
