# purescript-heme

Heterogeneous Mealy machines.

## Why?

The output types of Mealy machines sometimes change as the machine evolves. For example, it may produce a `String` followed by an `Int` followed by a `Unit`. This library provides a single function - `heme` - that helps build heterogeneous Mealy machines.

At Meeshkan, we're using this as an alternative to indexed monads. We've found that indexed monads are clunky and get in the way of a uniform, consistent and boring production stack. This lightweight solution allows for indexed behavior without the overhead of indexed monads.

## Todo

- Improve error messages so that they are at the point of function invocation rather than at the point of the constructino of `heme`.