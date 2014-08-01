# Underflow

This repository is an implementation of monads that provides:
* a trampolining monad 'harness' that prevents stack overflow
* an evil, stateful, fast version of the same harness that is interface-compatible
* a monad dictionary protocol for use with said harness

Underflow is not yet ready for prime time. Use with extreme caution.

Underflow began life as an implementation of Paul Graham's continuation-passing macros for Clojure, described in [On Lisp](http://www.paulgraham.com/onlisp.html).
This is combined with the observation that continuation-passing is a monad
(and in fact a [final in the category of monads](http://blog.sigfpe.com/2008/12/mother-of-all-monads.html)).
