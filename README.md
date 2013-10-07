## hurl

`hurl` is a Haskell high-level interface to the Chipmunk physics library in
early development. It is implemented in terms of `Hipmunk`, an excellent but
straightforward binding to Chipmunk. `hurl` intends to be more convenient and
idiomatic than `Hipmunk`.

## planned features

### use of pure data structures

In `Hipmunk`, you create entities in IO, then modify their properties using
StateVars. This very closely reflects the way this would be done in C.
In `hurl` you design bodies and shapes with pure ADTs, defering the IO
until later. Once they are added to the space they require IO to be modified,
but it is useful to have pure representations of the original objects.

### greater composability

In `Hipmunk`, you create bodies, shapes, and constraints. You then statefully
modify their properties, add them to a space, and define relationships between
them. It is difficult to compose these components because most of the operations
are done in the IO monad.

In `hurl` you work in terms of the `Object f` type. An `Object f` consists of a
`Body` and a traversable container `f` of `Solid`s. Objects can be combined
to create new objects, which remain pure and composable until you allocate the 
object in a `Space`. The space gives you back an immutable `ObjectRef f`, and
functions of the user-provided `f` can be used to refer to the immutable
`SolidRef`s that were created in the space.

### functional collision handling

`Hipmunk` exposes the Chipmunk API for registering callbacks in IO. An important
goal of `hurl` is to hide this callback interface and instead offer primitives
that can be used in functional reactive programming (FRP) frameworks.
