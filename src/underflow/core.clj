(ns underflow.core
  "Fast continuation-based nondeterministic fns.")

(defprotocol CFn
  (call [f stack v]))

(defprotocol State
  (push! [self f])
  (^clojure.lang.IFn pop! [self])
  (save! [self f])
  (continue! [self]))

(deftype StateImpl [stack continuation continuation-stack]
  State
  ; A slow stack for now
  (push! [_ cf]
    ;(prn "Pushing" cf)
    (swap! stack conj cf))
  (pop! [_]
    ;(prn "Popping" @stack)
    (when-let [r (first @stack)] (swap! stack rest) r))
  (save! [self closure]
    (reset! continuation-stack [stack continuation continuation-stack])
    (reset! continuation closure))
  (continue! [self]
    (if @continuation
      (let [[s2 c2 cs2] @continuation-stack
            c @continuation]
        (reset! stack s2)
        (reset! continuation c2)
        (reset! continuation-stack cs2)
        (c)))))

(defn new-state [] (StateImpl. (atom '()) (atom nil) (atom nil)))

(defn underflow [f arg]
  (let [state (new-state)]
    (loop [f f r arg]
      (if f
        (let [r (call f state r)]
          (recur (pop! state) r))
      r))))

(defmacro =defn [sym arg & body]
  (let [=sym (symbol (str "=" sym))]
    `(do
       (def ~sym
         (reify CFn
           (call [_ ~'*state* ~@arg] ~@body)))
       (defmacro ~=sym [arg#] `(call ~'~sym ~~''*state* ~arg#)))))

(defmacro =tailrecur [f & body]
  `(do
     (push! ~'*state* ~f)
     ~@body))

(defmacro =snap [body1-list [& args] body2-list]
  "Snaps the continuation in body2 in a closure, binds it to *state*,
  and executes body1. The bodies are lists that are spliced in."
  `(let [~'*state* (fn [~@args] ~@body2-list)] ~@body1-list))

(defmacro =shift [cvar & body]
  "Executes the given body binding the current continuation to cvar."
  `(let [~cvar ~'*state*] ~@body))

(defmacro =tailcall [f & args]
  `(fn [] (~f ~'*state* ~@args)))

(defmacro =tail-return [& values] `(fn [] (~'*state* ~@values)))

(defmacro =fn [args & body]
  `(fn ~(apply vector '*state* args) ~@body))

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
