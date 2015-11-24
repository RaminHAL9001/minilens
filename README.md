# Haskell Minimal Lenses

Lenses satisfy the properties of the Category type class, and are used
to inspect and update individual elements of a data structue in a type-safe
way. This library provides the simplest possible data types satisfying
these properties.

"minilens" type aims to provide a very simple improvement on Haskell's
record syntax: the idea of composable record accessors called 'Lens'es.

As of yet, there are no fancy Template Haskell APIs for generating lenses
programmatically; you are expected to write your lenses by hand. Some handy
helper functions are provided to help you do this.

This library was originally part of the Dao package, but has been branched
into it's own package in the hopes that it will be useful in a wider
variety of projects.

