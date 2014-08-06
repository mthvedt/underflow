(ns underflow.core
  (import (underflow.java Harness SafeHarness FastHarness Continuation)))

; DESIGN DECISIONS
; * Speed in Java comes from inlining and avoiding object construction.
; Underflow should offer both as options whenever possible.
; * Clojure is based on safety. Underflow should respect the safety/speed tradeoff.
; * Use of Underflow should be as low effort as possible, except when it interferes
; with the above two goals.

; IMPLEMENTATION DETAILS
; * Calls to the Harness are small monomorphic (or at worst dimorphic) calls,
; for maximum inlinability.
; * Most functionality is accomplished with macros reifying closures.
; This is preferred over types containing closures, to avoid the 'double dispatch'
; perf penalty. Any underflow operations should require no more than one polymorphic
; dispatch.

(defn tag [sym tag] (vary-meta sym assoc :tag tag))

(defn ^Harness safe-harness [] (underflow.java.SafeHarness. nil nil))
(defn ^Harness fast-harness [] (underflow.java.FastHarness. nil nil))

; TODO get-dict, set-dict macros

; Core macros
(defmacro =fn
  "Turn a fn of one argument into a state-passing fn."
  [[arg] & body]
  `(reify Continuation
     (call [~'_ ~'*state* ~arg] ~@body)))

(defmacro =named-fn
  [name [arg] & body]
  `(reify Continuation
     (call [self# ~'*state* ~arg]
       ; This fn should usually die in escape analysis. TODO TEST
       (let [~name (fn [arg# state#] (.call self# arg# state#))]
         ~@body))))

(defmacro =return
  [& body]
  `(.ret ~'*state* (do ~@body)))

(defmacro =call
  [f arg]
  (let [fs (gensym "f")]
    `(let [~(tag fs `Continuation) ~f]
       `(.call ~fs ~'*state* ~arg))))

(defmacro =>return
  "Like =return, but consumes stack in the fast implementation."
  [& body]
  `(.ter ~'*state* (do ~@body)))

(defmacro =with-cont
  "Totally overrides the old continuation. Use with caution."
  [cont & body]
  `(let [~'*state* (.setCont ~'*state* ~cont)] ~@body))

(defmacro =bindone
  [[bindingf bindingv] & body]
  `(let [old-cont# (.getCont ~'*state*)
         ~'*state* (.setCont ~'*state*
                             (=fn [~bindingf]
                                  (let [~'*state* (.setCont ~'*state* old-cont#)]
                                    ~@body)))]
     ~bindingv))

; Extended macros

(defmacro =letone
  [[bindingf bindingv] & body]
  `(=bindone [~bindingf (=return ~bindingv)] ~@body))

(defmacro =tailcall
  [& body]
  `(=letone [~'_ nil] ~@body))

(defmacro =bind [bindings & body]
  "Like let, but the binding vals may themselves be Underflow fns,
  and the continuation is saved as each binding executes."
  (if (empty? bindings)
    `(do ~@body)
    (let [tvar (first bindings)
          tval (second bindings)]
      (if-not (seq (rest bindings))
        (throw (IllegalArgumentException.
                 "=bind requires an even number of forms in binding vector"))
        `(=bindone [~tvar ~tval]
                   (=bind [~@(rest (rest bindings))] ~@body))))))

(defmacro =let [bindings & body]
  (if (empty? bindings)
    `(do ~@body)
    (let [tvar (first bindings)
          tval (second bindings)]
      (if-not (seq (rest bindings))
        (throw (IllegalArgumentException.
                 "=let requires an even number of forms in binding vector"))
        `(=letone [~tvar ~tval]
                  (=let [~@(rest (rest bindings))] ~@body))))))

(defmacro =declare [sym]
  `(do
     (declare ~(tag sym `Continuation))
     (defmacro ~(symbol (str "=" sym)) [arg#]
       `(.call ~'~sym ~~''*state* ~arg#))))

(defmacro =defn [sym [arg] & body]
  `(do
     (=declare ~sym)
     (def ~(tag sym `Continuation) (=fn [~arg] ~@body))))

; Underflow

(defmacro underflow
  [state & body]
  `(let [~'*state* ~(tag state `Harness)
         rval# (do ~@body)]
     (.extract ~'*state* (.execute ~'*state* rval#))))
