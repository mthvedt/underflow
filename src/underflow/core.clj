(ns underflow.core
  "Fast continuation-based nondeterministic fns.")

(defprotocol CFn
  (call [f stack v]))

; TODO nomenclature
(defprotocol SavedFn
  (continue [f stack]))

(defprotocol State
  ; Push a continuation frame.
  (push! [self f])
  ; Pop a continuation frame.
  (pop! [self])
  (error [self])
  (error? [self])
  ; Save the current continuation, along with an alternate f.
  (save! [self f])
  ; Pop the previously saved f and its continuation.
  (continue! [self]))

(deftype StateImpl [continuation-stack saved-f saved-states error?-state]
  State
  ; A slow stack for now
  (push! [_ cf]
    ;(prn "Pushing" cf)
    (swap! continuation-stack conj cf))
  (pop! [_]
    ;(prn "Popping" @continuation-stack)
    (when-let [r (first @continuation-stack)] (swap! continuation-stack rest) r))
  (save! [_ savedfn]
    ;(prn "Saving" savedfn)
    (reset! saved-states [@continuation-stack @saved-f @saved-states])
    (reset! saved-f savedfn))
  (error [_] (reset! error?-state true))
  (error? [_] @error?-state)
  (continue! [_]
    ;(prn "Continuing" @saved-f)
    (if @saved-f
      (let [[s2 c2 cs2] @saved-states
            sf @saved-f]
        (reset! continuation-stack s2)
        (reset! saved-f c2)
        (reset! saved-states cs2)
        (reset! error?-state false)
        sf))))

(defn new-state [] (StateImpl. (atom '()) (atom nil) (atom nil) (atom nil)))

(defn do-underflow
  [state r]
  ;(prn state r)
  (if-let [sfn (pop! state)]
    (let [r (call sfn state r)]
      (recur state r))
    r))

(defn underflow
  ; Calls underflow until state is exhausted
  [sfn arg]
  (let [s (new-state)
        r (call sfn s arg)
        r (do-underflow s r)]
    (if (error? s)
      (throw (RuntimeException. "Computation failed"))
      r)))

(defmacro =underflow [& body]
  `(let [~'*state* (new-state)
         result# (do ~@body)]
     (do-underflow ~'*state* result#)))

(defn do-underflow-seq [cfn]
  (let [state (new-state)
        v (continue cfn state)
        step (fn step [v]
               (loop [v v]
                 (if (error? state)
                   (if-let [c (continue! state)]
                     (recur (continue c state))
                     nil)
                   (let [r (do-underflow state v)]
                     (if (error? state)
                       (if-let [c (continue! state)]
                         (recur (continue c state))
                         nil)
                       (lazy-seq
                         (if-let [c (continue! state)]
                           (cons r (step (continue c state)))
                           (list r))))))))]
    (step v)))

(defmacro underflow-seq [& body]
  `(do-underflow-seq
     (reify SavedFn
       (continue [_ ~'*state*]
         ~@body))))

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
     (error ~'*state*)
     nil))

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
