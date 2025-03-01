# Fleet Array

Situation:

* All references to an immutable data structure have the same value
* Using immutable data structures makes it easier to reason about your programs
* Mutation can always be avoided by creating a modified copy instead.

Complication:

* Unfortunately, such copying has a high performance cost.
* In particular, if you only modify a small part of a large data structure, then the whole structure must be copied
* This can turn constant-time operations into linear-time operations.

Question:

* How can we avoid mutation without resorting to expensive copying?

Answer:

* Do the mutation, but keep track of changes
* Whenever the old version is used after the mutation, reapply the changes