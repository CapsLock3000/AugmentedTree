# Augmented Tree

**AugmentedTree** is F# library implementing immutable augmented red-black trees.
The structure `AugmentedTree` is designed to be a basis for other data structures
and algorithms.

All basic operations take O(log *n*) running time under some conditions
described in bibliography.

## Nomenclature

**Key** is value, over which we perform search.

**Attribute** is additional value attached to node, which is function
of current node key and attributes of node children.

## Bibliography

* Description of augmented search trees [(lecture)](http://www.bowdoin.edu/~ltoma/teaching/cs231/fall05/Lectures/augtrees.pdf).

* Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest, and Clifford Stein. 2009. *Introduction to Algorithms, Third Edition* (3rd ed.). The MIT Press. Chapter 14: Augmenting Data Structures.