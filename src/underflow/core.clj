(ns underflow.core
  "Fast continuation-based nondeterministic fns.")
; Safety beats speed.
; Eliminate 'statefn'.
; TODO maybe two ns, 'safe underflow' vs 'fast underflow'?

(defprotocol StateFn
  (call [f state v]))

; TODO eliminate protocols
; TODO save and restore impl? Need most efficient way...
(defprotocol State
  ; First task is to make basic underflow only understand push/pop.
  ; The wrapping underflow-seq can deal with save and restore.
  (get-cont [self])
  ; maybe push-cont returning a state?
  (set-cont [self c]))
  ;(snap-state [self])
  ;(unsnap-state [self state]))

(defprotocol BacktrackingState
  (get-snapshot [self])
  (set-snapshot [self s])
  (snap-stack [self])
  (unsnap-stack [self s]))

(defprotocol Snapshot
  (restart! [self state]))

; TODO maybe we can wrap state?
; TODO fast amb
;
; All snapshots are only run once. That's the rule.
(deftype BacktrackingStateImpl [^:unsynchronized-mutable cont
                                ^:unsynchronized-mutable snap]
  State
  (set-cont [_ c] (set! cont c))
  (get-cont [_] cont)
  BacktrackingState
  ; TODO figure out api here.
  (set-snapshot [_ s] (set! snap s))
  (get-snapshot [_] snap)
  (snap-stack [_] cont)
  (unsnap-stack [_ c] (set! cont c)))

(defn new-state [] (BacktrackingStateImpl. nil nil))

; Core macros

(defmacro =fn
  "Turn a fn of one argument into a state-passing fn."
  [[arg] & body]
  `(reify StateFn
     (call [~'_ ~'*state* ~arg] ~@body)))

(defmacro =push
  ; Warning: this is a 'side effect'!
  [[arg] & body]
  `(let [old-cont# (get-cont ~'*state*)]
     (set-cont ~'*state*
               (=fn [~arg]
                    (set-cont ~'*state* old-cont#)
                    ~@body))))

(defmacro =tailcall
  ;Execute body without consuming stack. Saves a stack frame
  ;at the expense of constructing a closure + Underflow call overhead.
  [& body]
  `(do
     (=push [~'_] ~@body)
     nil))

(defmacro =letone
  [[bindingf bindingv] & body]
  `(do
     (=push [~bindingf] ~@body)
     ~bindingv))

(defn save-helper [saved-stack-sym saved-snap-sym exprs]
  (if-let [expr (first exprs)]
    `(reify Snapshot
       (restart! [self# ~'*state*]
         (set-snapshot ~'*state*
                       ~(save-helper saved-stack-sym saved-snap-sym (rest exprs)))
         (unsnap-stack ~'*state* ~saved-stack-sym)
         ~expr))
    saved-snap-sym))

(defmacro =save-exprs! [& exprs]
  (let [saved-sym (gensym "stack-snap")
        snap-sym (gensym "snap")]
    `(let [~saved-sym (snap-stack ~'*state*)
           ~snap-sym (get-snapshot ~'*state*)
           snap# ~(save-helper saved-sym snap-sym exprs)]
       (set-snapshot ~'*state* snap#))))

; TODO save-values!

; Extended macros

(defmacro =save! [& body]
  `(=save-exprs! (do ~@body)))

; TODO retry that doesn't consume stack
(defmacro =retry []
  ; TODO don't need to pass state twice?
  `(when-let [s# (get-snapshot ~'*state*)]
     ;(prn "retrying" s#)
     (restart! s# ~'*state*)))

(defmacro =amb
  ([] `(=retry))
  ([body] body)
  ([body & bodies]
   `(do
      (=save-exprs! ~@bodies)
      ~body)))

(defn iterator-snapshot [iterator old-stack old-snap]
  (reify Snapshot
    (restart! [self *state*]
      (unsnap-stack *state* old-stack)
      (let [next (.next iterator)]
        (when-not (.hasNext iterator)
          ; when i is exhausted, remove us from the stack
          ; and restore the previous snapshot
          (set-snapshot *state* old-snap))
        next))))

(defmacro =apply-amb [iterable]
  `(let [saved# (snap-stack ~'*state*)
         snap# (get-snapshot ~'*state*)
         i# (.iterator (vary-meta ~iterable assoc :tag java.lang.Iterable))
         newsnap# (iterator-snapshot i# saved# snap#)]
     (if (.hasNext i#)
       (do
         (set-snapshot ~'*state* newsnap#)
         (.next i#))
       (=retry))))

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

(defmacro =declare [sym]
  `(do
     (declare ~sym)
     (defmacro ~(symbol (str "=" sym)) [arg#] `(call ~'~sym ~~''*state* ~arg#))))

(defmacro =defn [sym [arg] & body]
  `(do
     (=declare ~sym)
     (def ~sym (=fn [~arg] ~@body))))

; Underflow

(defn do-underflow
  [state start-value]
  (if-let [sfn (get-cont state)]
    (let [r (call sfn state start-value)]
      (recur state r))
    start-value))

(defn underflow
  ; Calls underflow until state is exhausted
  [sfn arg]
  (let [s (new-state)
        r (call sfn s arg)]
    (do-underflow s r)))

(defmacro =underflow [& body]
  `(let [~'*state* (new-state)
         result# (do ~@body)]
     (do-underflow ~'*state* result#)))

(deftype UnderflowIterator [^:unsynchronized-mutable nextv state]
  java.util.Iterator
  (hasNext [_]
    ;(prn "Has next " (get-snapshot state))
    (if (get-snapshot state) true false))
  ; Needs to be called once by underflow-iterator, to set the first nextv
  (next [_]
    ;(prn "Nextv is " nextv " snapshot is " (get-snapshot state))
    ; Make sure this nextv didn't come from the terminal snapshot
    (if-let [snap (get-snapshot state)]
      (let [r nextv]
        (set! nextv (do-underflow state (restart! snap state)))
        r)
      (throw (java.util.NoSuchElementException.))))
  (remove [_]
    (throw (UnsupportedOperationException.))))

(defn do-underflow-iterator [snapshot]
  (let [state (new-state)
        _ (set-snapshot state snapshot)
        r (UnderflowIterator. nil state)]
    (.next r)
    r))

; Used to gracefully terminate at the end of a computation, by returning nil.
(def terminal-snapshot
  (reify Snapshot
    (restart! [self state]
      ;(prn "terminal snapshot" self)
      ; TODO this is impl dependent
      (set-snapshot state nil)
      (unsnap-stack state nil)
      nil)))

(defmacro initial-snapshot [& body]
  `(reify Snapshot
     (restart! [self# ~'*state*]
       ;(prn "initial snapshot" self#)
       (set-snapshot ~'*state* terminal-snapshot)
       ~@body)))

(defmacro underflow-iterator [& body]
  `(do-underflow-iterator (initial-snapshot ~@body)))

(defmacro underflow-seq [& body]
  ; TODO check thread safety of this
  `(iterator-seq (underflow-iterator ~@body)))
