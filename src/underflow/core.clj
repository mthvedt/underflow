(ns underflow.core
  "Fast continuation-based nondeterministic fns.")

(defprotocol StateFn
  (call [f state v]))

; TODO eliminate protocols
; TODO save and restore impl? Need most efficient way...
(defprotocol State
  ; First task is to make basic underflow only understand push/pop.
  ; The wrapping underflow-seq can deal with save and restore.
  (get-cont [self])
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
; BackTrackingStateImpl underlying-state snapshot-stack
; Since we have to construct a new snapshot every time...
; could make it a linked list...
;
; Does this apply to stacks? Should they be linked lists?
; Probably? It's the simpler, general purposer solution.
; It seems rare that a continuation won't capture its closed over environment.
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

; TODO amb iterable, amb vals
(defmacro =save! [& body]
  `(let [saved# (snap-stack ~'*state*)
         old-snap# (get-snapshot ~'*state*)
         snap# (reify Snapshot
                 (restart! [self# ~'*state*]
                   (set-snapshot ~'*state* old-snap#)
                   (unsnap-stack ~'*state* saved#)
                   ;(prn "restarting saved" self#)
                   ~@body))]
     (set-snapshot ~'*state* snap#)))

(defmacro =amb
  ; TODO does this have the correct tail semantics? does it matter?
  ([closure-body] closure-body)
  ([body & bodies]
   `(do
      (=save! (=amb ~@bodies))
      ~body)))

; TODO retry that doesn't consume stack
(defmacro =retry []
  ; TODO don't need to pass state twice
  `(when-let [s# (get-snapshot ~'*state*)]
     ;(prn "retrying" s#)
     (restart! s# ~'*state*)))

; Extended macros

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
  `(iterator-seq (underflow-iterator ~@body)))
