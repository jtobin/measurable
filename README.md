measurable
----------

*measurable* is a simple shallowly-embedded DSL for dealing with measures.

It uses a `Measure` synonym for a standard continuation type with a restricted
output type and no `callCC` implementation.  You can construct measures from
samples, mass/density functions, or even sampling functions.

Construct image measures by `fmap`-ing measurable functions over them, or
create new measures from existing ones by measure convolution and friends
provided by a simple `Num` instance enabled by an `Applicative` instance.
Create measures from graphs of other measures using the `Monad` instance and
do-notation.

Query measures by integrating meaurable functions against them.  Extract
moments, cumulative density functions, or probabilities.

Check out the module comments or **examples** folder for sample use.

Caveat: while fun to play with, and rewarding to see how measures fit together,
measure operations as nested integrals are exponentially complex.  Don't expect
them to scale very far!


