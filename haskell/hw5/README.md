# Treap

### 1. Short description of the Treap data structure

[Wikipedia article](https://en.wikipedia.org/wiki/Treap)

The word Treap is a combinaion of Heap and Tree, because the data structure is esentially a binary
search tree where one can search by keys, but for each node a *random* priority is chosen, for which
we want heap-properties to be in place.
This means we want the priority of each node to be greater than the priority of each of its children.
The randomness gives us a relatively uniform distribution when adding new nodes, meaning the treap
stays relatively balanced allowing logarithmic searching.

The main operations on a treap are:
* check if empty
* searching for a key
* adding a key
* removing a key

### 2. Base module `Treap.hs`

This module contains the main logic behind each of the main operations on a Treap.
It contains only pure functions (not IO) meaning no random numbers can be used.

Main aspects of this module:
* type Key, arbitrary type (some functions need it to be an instance of `Eq`, `Ord` or `Show`)
* type Priority, a whole number in range [1; 10 000]
* type Element, a tuple of key and priority
* algebraic type Treap - either an empty treap or a treap with root some Element and left and right children - again treaps

#### `empty :: Treap a → Bool`
Is the Treap empty?

Determined according to its constructor.

#### `contains :: Treap a → Key a → Bool`
Does the Treap contain the given Key?

Searched as in a binary search tree.

#### `add :: Treap a → Element a → Treap a`
Add an Element to the Treap.

1) According to the Key, find the position of the Element as a leaf, like adding to a binary search tree.
2) According to the Priority, perform rotations to preserve heap properties.

Uses the functions `fixLeftPriority` and `fixRightPriority` for standard binary search tree rotations.

Returns a new Treap with the Element added.

#### `delete :: Treap a → Key a → Treap a`
1) Searches for the Key according to the binary serach tree algorithm.
2) Separate cases are considered if the node turns out to be a leaf / have one child / have two children.
3) Uses the private functions `leftMost` and `rightMost`, that take a Treap and return its left most / right most element.

#### `toList :: Treap a → [a]` and `toString :: Treap a → String`
Take a Treap and return a 'left-root-right' traversal of its keys in the according format.

#### `rotateToString :: Treap a → Int → Bool → Bool → String`
Takes a Treap and a few options a returns a visualization of the Treap as a string.

Each key is on a separate line and is indented according to its depth in the Treap.

The ordering is left-root-right.

### 3. Additional module `TreapIO.hs`
This module provides the main characteristic of the Treap - the random prioritues.
This comes at the cost of all functions being IO, meaning they can no be used in pure functions.

`TreapIO.hs` contains IO equivalents of the functions in `Treap.hs`.
The user is supposed to import and use `Treap.hs` and `TreapIO.hs` together.

The main structure of the functions in this module is:
1) take an IO object as input.
2) unpackaging / connecting the IO object with a regular one
3) passing the regular object to the according pure function from `Treap.hs`
4) returning the result again packaged in an IO object

The only difference is in the `add` functions, which apart from the actions listed above, generates
a pseudo-random number in the interval [1, 10 000] with `System.Random` and uses it as a priority
for adding the key.

### 4. Tests in `main.hs`

#### `testTreapOfDifferentTypes`
Creates two Treaps with types of the keys `Int` and `Char` and prints them.

You can see a commented line which tries to add a `Char` key to an `Int` Treap, which would not compile.

#### `testPrintRepresentations`
Creates an example `ioTreap` and prints it as a string and as a list.

#### `testTreapIO`
Calls the main four treap functions with example data.

#### `compareSameVsRandomPriorities`
Creates a `Treap` of keys with same priorities and an `ioTreap` with randomly chosen ones and prints
them in order to deomnstrate that the `Treap`'s height is linear, whereas the `ioTreap`'s is logarithmic.
