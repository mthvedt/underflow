# Underflow

Underflow is a very fast implementation of McCarthy's `amb` operator for clojure. It has:

* a trampolining monad 'harness' that prevents stack overflow
* an evil, stateful, fast version of the same harness that is interface-compatible
* various macros for various `amb` operations using the harness

Underflow is not yet ready for prime time. Use with extreme caution.

Underflow began life as an implementation of Paul Graham's continuation-passing macros for Clojure, described in [On Lisp](http://www.paulgraham.com/onlisp.html).
This is combined with the observation that continuation-passing is a monad
(and in fact a [final in the category of monads](http://blog.sigfpe.com/2008/12/mother-of-all-monads.html)).
