(ns underflow.core
  "Fast continuation-based nondeterministic fns.")

(defprotocol CFn
  (call [f stack v]))

(defprotocol SavedFn
  (continue [f stack]))

(defprotocol State
  (push! [self f])
  (pop! [self])
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
    (reset! continuation-stack [@stack @continuation @continuation-stack])
    (reset! continuation closure))
  (continue! [self]
    (if @continuation
      (let [[s2 c2 cs2] @continuation-stack
            c @continuation]
        (reset! stack s2)
        (reset! continuation c2)
        (reset! continuation-stack cs2)
        (continue c self)))))

(defn new-state [] (StateImpl. (atom '()) (atom nil) (atom nil)))

(defn do-underflow
  [state r]
  ;(prn state r)
  (if-let [cfn (pop! state)]
    (let [r (call cfn state r)]
      (recur state r))
    r))

(defn underflow
  [cfn arg]
  (let [s (new-state)]
    (push! s cfn)
    (do-underflow s arg)))

(defn underflow-seq [cfn arg]
  (let [state (new-state)
        _ (push! state cfn)
        step (fn f [r]
               (lazy-seq
                 ;(prn "Lazy seq arg" r)
                 (when-let [v (do-underflow state r)]
                   ;(prn "Lazy seq body" v state)
                   (cons v (f (continue! state))))))]
    (step arg)))

(defmacro =underflow [& body]
  `(let [~'*state* (new-state)
         result# (do ~@body)]
     (do-underflow ~'*state* result#)))

(defmacro =defn [sym [arg] & body]
  (let [=sym (symbol (str "=" sym))]
    `(do
       (declare ~sym)
       (defmacro ~=sym [arg#] `(call ~'~sym ~~''*state* ~arg#))
       (def ~sym
         (reify CFn
           (call [_ ~'*state* ~arg] ~@body))))))

; Core macros

(defmacro =tailrecur [f & body]
  `(do
     (push! ~'*state* ~f)
     ~@body))

(defmacro =fn [[arg] & body]
  `(reify CFn
     (call [_ ~'*state* ~arg] ~@body)))

(defmacro =call [expr1 argv expr2]
  `(do
     (push! ~'*state* (=fn ~argv ~expr2))
     ~expr1))

(defmacro =save [rval-body & closure-body]
  `(let [closure# (reify SavedFn
                    (continue [_ ~'*state*] ~@closure-body))]
     ;(prn "Saving" ~'*state* closure#)
     (save! ~'*state* closure#)
     ~rval-body))

(defmacro =retry []
  `(do
     ;(prn "Continuing" ~'*state*)
     (continue! ~'*state*)))

; Extended macros

(defmacro =letone [[bindingf bindingv] & body]
  "Helper macro for =let. Use =let in preference to using this directly."
  `(=call ~bindingv [~bindingf] (do ~@body)))

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
