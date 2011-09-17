(ns underflow.core)

; TODO split into blog post branch?
(defmacro reset= [thefn & args]
  "Sets a continuation context for the given fn, supplying identity as the first
  continuation."
  `(trampoline ~thefn identity ~@args))

(defmacro =snap [body1-list [& args] body2-list]
  "Snaps the continuation in body2 in a closure, binds it to *cont*,
  and executes body1. The bodies are lists that are spliced in."
  `(let [~'*cont* (fn [~@args] ~@body2-list)] ~@body1-list))

(defmacro =shift [cvar & body]
  "Executes the given body binding the current continuation to cvar."
  `(let [~cvar ~'*cont*] ~@body))

(defmacro =tailcall [f & args]
  `(fn [] (~f ~'*cont* ~@args)))

(defmacro =defn [sym args & body]
  "Defines an fn prepending *cont* to its list of arguments. Also defines
  a = version that sets a continuation context."
  (let [=sym (symbol (str "=" sym))]
    `(do
       (defmacro ~=sym [~'& args#] `(~'~sym ~~''*cont* ~@args#))
       (defn ~sym [~'*cont* ~@args] ~@body))))

(defmacro =return [& values] `(~'*cont* ~@values))

(defmacro =tail-return [& values] `(fn [] (~'*cont* ~@values)))

(defmacro =fn [args & body]
  `(fn ~(apply vector '*cont* args) ~@body))

(defmacro =letone [[bindingf bindingv] & body]
  "Helper macro for =let. Use =let in preference to using this directly."
  `(=snap ~(list bindingv) [~bindingf] ~body))

(defmacro =let [bindings & body]
  "Like let, but the binding vals may themselves be Underflow fns,
  and the continuation is saved as each binding executes."
  (if (empty? bindings)
    `(do ~@body)
    (let [tvar (first bindings)
          tval (second bindings)]
      (if (nil? tval)
        (throw (IllegalArgumentException.
                 "=let requires an even number of forms in binding vector"))
        `(=letone [~tvar ~tval]
                  (=let [~@(rest (rest bindings))] ~@body))))))

       ; TODO
#_(defmacro >continue [state cont rval]
  "Calls the given continuation with the given value.

  N.B.: This wipes out any continuation state that is currently present."
  `(let [cont# ~cont
         rval# ~rval]
     (reify Continuer
       (process [_ state#]
         (if cont#
           (cont# rval#)
           rval#)))))

#_(defmacro =do
    "Executes severeal forms, one after another. The forms may be Underflow forms."
    ([] nil)
    ([b] b)
    ([b1 b2 & body]
     (reify Continuer
       (process [_ state#]
         (let [old-cont# (cont state#)]
           (do
             (set-cont! state#
                        (fn [_]
                          (reify Continuer
                            (process [_ state#]
                              (set-cont! state# old-cont#)
                              (=do ~b2 ~@body)))))
             ~b1))))))

#_(defmacro =suspend []
    "Escapes the current Underflow trampoline, returning a fn of one arg
    encapsulating the current delimited continuation."
    `(reify Continuer
       (process [_ state#]
         (let [state2# (fork state#)]
           (set-cont! state# nil)
           (fn [x] (underflow-helper x state2#))))))

#_(defmacro cmacro [sym argl doc & body]
  `(do
     ~(binding [x y z] `(...))
     ~(binding [x y z] `(...))))
