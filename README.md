# hs-partition-example
An example using Haskell partition and FGL library to construct a lattice structure (as a FGL graph) of substutitions (as paritions) starting from the set where every element is distinct and finally down to where all elements are unified.
When you run the `main` function in `app/Main.hs` with ghci, a Graphviz preview diagram will appear, which is also provided as a captured image below:

![preview](https://raw.githubusercontent.com/kyagrd/hs-partition-example/master/preview.png)

Trivial equivalence classes (i.e., singleton sets) in a partition are omitted from the output.
That is, the top node `[]` really means `[[0],[1],[2],[3]]`.

Hackage pages of the libraries used
 * data-parition https://hackage.haskell.org/package/data-partition
 * fgl https://hackage.haskell.org/package/fgl
 * graphviz https://hackage.haskell.org/package/graphviz

WARNING: Space complexity grows really fast. Preview works up to size 5 but at size 6 is two large for an image. From 7 it requires huge amount of memory and likely to segfault even before generating the preview.
