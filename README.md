# Fleet Array

Situation:

* There is one essential idea that I must explain properly. Usually in Haskell
  you think of everything as a value. However, under the hood most things are
  implemented using references. Everytime I say "old version" or "old
  reference", I refer to this implementation detail. There is no way to really
  explain it without explaining how references are used in the implementation of
  Haskell.

* Using immutable data structures makes it easier to reason about your programs
  (motivation)
* All references to an immutable data structure have the same value
  (restriction)
* Mutation can always be avoided by creating a modified copy instead (bad
  previous solution).
* Copying can be cheap, if the data that needs to be copied is not much larger
  than the data that has been changed
* That happens, for example, when changing the first element of a linked list.
  The remainder of the list can be shared in memory.

Complication (why is it bad?):

* Arrays, on the other hand, are an example of a data structure where changes
  can be much smaller than the copied data.
* In an array, changing a single element would necessitate a copy of the entire
  array.
* Thus, such copying unfortunately has a high performance cost.
* Copying can turn constant-time operations into linear-time operations (and
  linear-time into quadratic-time, etc.).

Question:

* How can we avoid mutation without resorting to expensive copying?

Answer:

* Do the mutation, but keep track of changes
* Old references to the same data will be mutated in an equal but opposite way
* Whenever the old version is used after the mutation, reapply the changes

