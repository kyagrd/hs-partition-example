# hs-partition-example
An example using Haskell partition and FGL library to construct a lattice structure (as a FGL graph) of substutitions (as paritions) starting from the set where every element is distinct and finally down to where all elements are unified.
When you run the `main` function in `app/Main.hs` with ghci, Graphviz preview diagram will appear, which is also provided as a captured image below:

![preview](https://raw.githubusercontent.com/kyagrd/hs-partition-example/master/preview.png)

Trivial partitions of singleton sets are omitted. That is the top node `[]` means `[[0],[1],[2],[3]]`.

Hackage pages of the libraries used
 * data-parition https://hackage.haskell.org/package/data-partition
 * fgl https://hackage.haskell.org/package/fgl
 * graphviz https://hackage.haskell.org/package/graphviz
