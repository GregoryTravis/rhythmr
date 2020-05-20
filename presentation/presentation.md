# Rhythmr: Like Tinder, But For Def Beats

---

# What is a def beat
  * List: def, slammin’, rockin’, tight, etc

---

# Stacking loops
  * Stack of loops
  * Makes a beat

---

# What makes loops go well together?
  * Line up?
  * Waveforms with lines showing coincidence

---

# What makes loops go well together?
  * Interesting syncopation?
  * Waveforms with lines showing non-coincidence

---

# It’s subjective
  * I’m not particularly interested in finding out if there is an algorithm

---

# Done by hand
  * Complex JBE doc

---

# There’s another way: rapid trial and error
  * Generate combinations and pick the ones you like
  * I happen to particularly like generate-and-select, especially when you can get through a large set of possibilities

---

# Randomly
  * Block of loops and combos, with arrows
  * This works, but it does take a while
  * I did this once with a few thousand mini-songs made from a few hundred loops, and I picked out about 30 I liked; each one was about 15 seconds, so it took a while

---

# How to represent a beat?
  * Block of beats

---

# How to represent a beat?
  * Fade out all but four

---

# How to represent a beat?
  * 2^128 = [big number]

---

# How to represent a beat?
  * All possible subsets of 8
  * Even with just 8, there are a lot of possibilities
  * There’s no structure in this layout

---

# How about a graph?
  * Graph with a few groups (complete subgraphs)
  * This is O(n^2), much better than O(2^n)

---

# How about a graph?
  * Graph with positive and negative edges (red and green?)

---

# How about a graph?
  * Counterexample: three that sound good together, but all the pairs do not
  * 3-vertex graph complete in both red and blue

---

# Transitivity
  * This brings up an interesting question
  * Chain of loops
  * If A+B, B+C, …, then does it follow that A+Z?
  * The answer seems to be that transitivity holds more often for smaller groups [is that actually what I’ve found?]

---

# Transitivity
  * A..Z as a chain in a graph
  * Maybe this doesn’t represent a group, but rather a set of pairwise affinities

---

# Hypercube (1D)
  * Block of 1 loop off to the side
  * Line segment, left is no loop, right is yes loop
  * 0 and 1

---

# Hypercube (2D)
  * Block of 2 loops off to the side
  * Square, lr is loop 0, ud is loop 1
  * 00..11

---

# Hypercube (3D)
  * Etc for 3
  * Now we have a choice: 4 groups (of >1)

---

# Hypercube (4D)
  * Etc for 4

---

# Hypercube (16D)
  * Etc for 16, crazy
  * Finding groups is walking along the edges of a hypercube of some high dimension

---

# Hypercube (local neighborhood) [??]
  * 4D hypercube, one selected vertex, some neighbors
  * Adding a loop is moving along an edge, or perhaps rotating the hypercube
  * Same with removing

---

# Hypercube: Rejected
  * A hypercube with 128 dimensions is just too visually noisy
  * Turns out it's pretty tricky to animate 128d rotation

---

# Lattice
  * Lattice of 2
  * Nodes are labeled with little groups, some filled in, some empty

---

# Lattice
  * Lattice of 3

---

# Lattice
  * Lattice of 4

---

# Lattice
  * Lattice of large number
  * This isn’t super-enlightening

---

# Subset
  * However, this does bring up another interesting question
  * Series of subsets with the subset symbol
  * If three loops sound good together, do two of them sound good together? I say the answer is “yes”, or perhaps “usually”
  * (Audio)
  * Another series of subsets, maybe the other dir?
  * Conversely, if two loops sound bad together, do three sound bad? I say the answer is “yes”, or perhaps “usually”
  * (Audio)

---

# Lattice
  * On a lattice, subsets are connected by edges

---

# Lattice
  * Highlight a 3 and it’s 2 subset, with groups off to the right
  * Moving up the lattice from a good beat should also be good

---

# Lattice
  * Same but the other way
  * Moving down the lattice from a bad beat should also be bad

---

# Lattice
  * This can eliminate beats to check -- pruning the tree, if you will
  * Show a pruned sublattice

---

# Heuristics
  * A precise visualization of these immense spaces is hard, but showing affinity groups works pretty well

---

# Heuristics (have a slide for each one)
  * The Subset Rule
  * The Superset Rule
  * The Transitive Rule
  * The Exception Rule

---

# The tool
  * With these principles in mind, here’s the tool
  * Pool
  * Affinities
  * Song
  * There's the hypercube in the background
  * Swiping left and right
  * Note the strategy
  * Picking the strategy with the other keys
  * Rendering a song
      * The score
  * “”
      * The score and some loops, lines connecting them
      * (Audio)
  * FX
      * For added fun, we do more than just stack them
      * Loops, but now some FXed ones -- some visual cue
  * “”
      * A score with FX
      * (Audio)
---

# Further Work
  * Web site: a kind of social network for beats and beat-makers
      * You get reputation for good/utilized loops, groups, scores, and search terms
