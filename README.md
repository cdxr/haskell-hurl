## hurl

`hurl` is a Haskell high-level interface to the Chipmunk physics library in
early development. It is implemented in terms of `Hipmunk`, an excellent but
straightforward binding to Chipmunk. `hurl` intends to be more convenient and
idiomatic than `Hipmunk`.

## planned features

### use of pure data structures

In Hipmunk, as in C, you create entities in IO, then modify their properties
using StateVars. Hurl enables you to design bodies and shapes with pure ADTs,
defering the IO until later.

### greater composability

In Chipmunk, you create bodies, shapes, and constraints. You then statefully
modify their properties, add them to a space, and define relationships between
them.

In `hurl`, you work in terms of the `Object f` type. An object consists of a
`Body` and a traversable container `f` of `Solid`s. Objects can be combined
to create new objects, which remain pure and composable until you allocate a
copy in the `Space`. The space gives you back an immutable `ObjectRef f`, and
the user-provided `f` can be used to refer to specific `SolidRef`s.

### functional collision handling

`Hipmunk` exposes the Chipmunk API for registering callbacks in IO. An ultimate
goal of `hurl` is to hide this callback interface and instead offer primitives
that can be used in functional reactive programming (FRP) frameworks.
