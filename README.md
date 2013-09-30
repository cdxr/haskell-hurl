# hurl

`hurl` is a Haskell high-level interface to the Chipmunk physics library. It is
implemented in terms of `Hipmunk`, an excellent but straightforward binding to
Chipmunk. `hurl` intends to be more convenient and idiomatic than `Hipmunk`,
even in ways that sacrifice performance.

Hurl is in the early stages of development, and intends to provide the following
advantages over Hipmunk:

## Use of pure data structures

In Hipmunk, as in C, you create entities in IO, then modify their properties
using StateVars. Hurl enables you to design bodies and shapes with pure ADTs,
defering the IO until later.

## Greater composability

In chipmunk, you create bodies, shapes, and constraints. Then you statefully
modify their properties. You then add them to a space, and statefully define
relationships between them.

Instead in `hurl` you work in terms of Objects, which consist of a body and an
arbitrary traversable container of shapes. Objects may be composed freely with
their applicative instance. The final object is then added to a space, which
returns an immutable ObjectRef.
