
# measurable

[![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/jtobin/measurable/blob/master/LICENSE)

**measurable** is a simple shallowly-embedded DSL for dealing with measures.

It uses a `Measure` synonym for a standard continuation type with a restricted
output type and no `callCC` implementation.  You can construct measures from
samples, mass/density functions, or even sampling functions.

Construct image measures by `fmap`-ing measurable functions over them, or
create new measures from existing ones by measure convolution and friends
provided by a simple `Num` instance enabled by an `Applicative` instance.
Create measures from graphs of other measures using the `Monad` instance and
do-notation.

Query measures by integrating measurable functions against them.  Extract
moments, cumulative density functions, or probabilities.

You can check out a few blog posts I wrote about the theoretical foundations
and implementation of the library here:

* [Foundations of the Giry Monad][foun]
* [Implementing the Giry Monad][impl]
* [The Applicative Structure of the Giry Monad][appl]

A more polished and extended version of the above appears in chapter three of
[my dissertation][diss].

Caveat: while fun to play with, and rewarding to see how measures fit together,
measure operations as nested integrals are exponentially complex.  Don't expect
them to scale very far!

[diss]: https://jtobin.io/assets/jtobin-dissertation.pdf
[foun]: https://jtobin.io/giry-monad-foundations
[impl]: https://jtobin.io/giry-monad-implementation
[appl]: https://jtobin.io/giry-monad-applicative
