## hurl

`hurl` is a high-level interface to the Chipmunk physics library.
It is implemented in terms of `Hipmunk`, an excellent but straightforward
Haskell binding to Chipmunk. `hurl` is designed to provide a more idiomatic
interface than `Hipmunk`.

### advantages over Hipmunk

#### greater composability

In `Hipmunk`, you create entities in IO, then modify their properties using
StateVars. This very closely reflects the way this would be done in C.

Instead in `hurl` you design bodies and shapes in terms of `Solid`s, pure ADTs
that are composable and lazy. Solids have physical properties such as density
and volume, so the user may add them to spaces without manually computing the
mass and moment for the body. Those properties are computed lazily, so the user
may add a Solid to a static body without incurring any unneeded computation.

#### greater safety

* The type system prevents the user from applying forces to static bodies. To
move static bodies it is necessary to do so in a context that will ensure that
the static hashes are recomputed.

* It is not possible to cause a segfault by forgetting to initialize Chipmunk.
This is done automatically the first time a `Space` is created.

* *TODO* It is not possible to cause a segfault by performing operations on
a freed `Space` or `Object`.


### planned features

#### functional collision handling

`Hipmunk` handles collisions by registering IO callbacks. An important
goal of `hurl` is to hide this callback interface and instead offer primitives
that can be used in functional reactive programming (FRP) frameworks.
