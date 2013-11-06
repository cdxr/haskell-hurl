## hurl

`hurl` is a high-level interface to the Chipmunk physics library.
It is implemented in terms of `Hipmunk`, an excellent but straightforward
Haskell binding to Chipmunk. `hurl` is designed to provide a more idiomatic
interface than `Hipmunk`.

### advantages over Hipmunk

#### greater composability

In `Hipmunk`, you create entities in IO, then modify their properties using
StateVars. This very closely reflects the way this would be done in C.

Instead in `hurl` you design bodies and shapes in terms of `Solid`s, a pure ADT
that is composable and lazy. Solids have physical properties such as density
and volume, so the user may transform and compose them in different ways before
adding them to a space, and the mass and moment for the body is computed
automatically. Those properties are computed lazily, so the user may build up a
complex Solid and add it as a static body without wasting any time computing
the unused physical properties.

Solids can also be transformed isometrically. This includes translation,
rotation, and reflection. They can also be scaled uniformly. Scaling is
distinguished from isometric transformations at the type level, because each
Solid has a cached volume that is used to compute its mass. Volume is invariant
under isometric transformations, so the type prevents the cache from being
needlessly recomputed.

#### better safety

* The type system prevents the user from applying forces to static bodies. To
move static bodies it is necessary to do so in a context that will ensure that
static hashes are recomputed (forgetting to do this in Hipmunk would result in
unintended behavior).

* It is no longer necessary to initialize Chipmunk. This is done automatically
the first time a `Space` is created.

* The user no longer needs to manually allocate group ids to Shapes to prevent 
them from colliding. Groups are automatically managed by the `Space` type
(in progress).

* All of the operations defined in `Hipmunk` that could result in segfaults have
been encapsulated and instead will throw exceptions (in progress).


### disadvantages compared to Hipmunk

* GHC-only due to the use of language extensions

* Reduced space efficiency for some operations


### planned features

#### functional collision handling

`Hipmunk` handles collisions by registering IO callbacks. An important
goal of `hurl` is to hide this callback interface and instead offer primitives
that can be used in functional reactive programming (FRP) frameworks.
