(ns underflow.core
  "Fast continuation-based nondeterministic fns."
  (import java.util.Iterator java.lang.Iterable))
; TODO:
; Prefer types with closures over macros.

; DESIGN DECISIONS
; * Speed in Java comes from inlining and avoiding object construction.
; Underflow should offer both as options whenever possible.
; * Clojure is based on safety. Underflow should respect the safety/speed tradeoff.
; * Use of Underflow should be as low effort as possible, except when it interferes
; with the above two goals.

; IMPLEMENTATION DETAILS
; * It is assumed that callers will treat States monomorphically.
; Each call to a state fn is assumed to be an inlinable static call.
; This reduces the amount we need to worry about State performance.
; * Most functionality is accomplished with macros reifying closures.
; This is preferred over types containing closures, to avoid the 'double dispatch'
; problem. Any underflow operations should require no more than one polymorphic
; dispatch. In some places, we lean on the JVM's ability to infer static bindings,
; and hence use types containing closures.

(defprotocol State
  (get-cont [self])
  (set-cont [self c])
  (return [self other-state r])
  (execute [self other-state snapfn]))
  ;(snap-state [self])
  ;(unsnap-state [self state]))

(defprotocol BacktrackingState
  (get-snapshot [self])
  (set-snapshot [self s])
  (snap-stack [self])
  (unsnap-stack [self s]))

(defprotocol Snapshot
  ; TODO maybe this should assoc itself? is state the state or just a dict?
  (get-state [self])
  (restart! [self state]))

; Marker type
(defprotocol SafeStateRval
  (safe-state-rval-execute [self]))
; Separate deftype to ease debugging stack traces
(deftype SafeStateRvalImpl [f s v]
  SafeStateRval
  (safe-state-rval-execute [self]
    (f s v)))

(defprotocol SafeBacktracker
  (safe-backtracker-rval [self])
  (next-snap [self]))
(deftype SafeBacktrackerImpl [r s]
  SafeBacktracker
  (safe-backtracker-rval [_] r)
  (next-snap [_] s))

(defn check-obsolete [obsolete]
  (if obsolete
    (throw (IllegalStateException. "Using old reference"))))

; TODO this is actually a generic stateful computation.
(deftype SafeState [cont statev ^:unsynchronized-mutable obsolete]
  State
  (get-cont [_]
    (check-obsolete obsolete)
    cont)
  (set-cont [_ c]
    (set! obsolete true)
    (SafeState. c statev nil))
  (return [self other-state r]
    ; TODO underflow identity isn't doing its thing properly.
    ; how to faster terminate execution?
    (if-let [cont (get-cont other-state)]
      (SafeStateRvalImpl. cont other-state r)
      (SafeBacktrackerImpl. r statev)))
  ; TODO peformance test this.
  ; The snapfn should be stack allocated in most circumstances.
  (execute [self other-state snapfn]
    ; A snapfn is basically a Haskell cont monad--it deposits a value
    ; into a continuation.
    (loop [v (restart! snapfn other-state)]
      (if (instance? underflow.core.SafeStateRval v)
        (recur (safe-state-rval-execute v))
        v)))
  BacktrackingState
  (set-snapshot [_ s]
    (set! obsolete true)
    (SafeState. cont s nil))
  (get-snapshot [_]
    (check-obsolete obsolete)
    statev)
  (snap-stack [self]
    (check-obsolete obsolete)
    (get-cont self))
  (unsnap-stack [self c]
    (set-cont self c)))

(deftype MutableState [^:unsynchronized-mutable cont]
  State
  (get-cont [_] cont)
  (set-cont [self c] (set! cont c) self)
  (return [_ _ r] r)
  (execute [self other-state snapfn]
    ; TODO this might be broken
    (loop [v (snapfn other-state)]
      (if cont
        (recur (cont other-state v))
        v))))

; All snapshots are only run once. That's the rule.
; TODO snapshot cloning
(deftype FastBacktrackingState [^:unsynchronized-mutable state
                                ^:unsynchronized-mutable snap]
  State
  (set-cont [self c] (set! state (set-cont state c)) self)
  (get-cont [_] (get-cont state))
  (return [_ other-state r] (return state other-state r))
  (execute [self other-state v] (execute state other-state v))
  BacktrackingState
  (set-snapshot [_ s] (set! snap s))
  (get-snapshot [_] snap)
  (snap-stack [self] (get-cont self))
  (unsnap-stack [self c] (set-cont self c)))

;(defn new-state [] (FastBacktrackingState. (SafeState. nil) nil))
;(defn new-state [] (FastBacktrackingState. (MutableState. nil) nil))
(defn new-state [sfn] (SafeState. sfn nil nil))
;(defn new-state [sfn] (MutableState. sfn))

; Core macros

(defmacro =fn
  "Turn a fn of one argument into a state-passing fn."
  [[arg] & body]
  `(fn [~'*state* ~arg] ~@body))

(defmacro =return
  [& body]
  `(return ~'*state* ~'*state* (do ~@body)))

(defmacro =bindone
  [[bindingf bindingv] & body]
  `(let [old-cont# (get-cont ~'*state*)
         ~'*state* (set-cont ~'*state*
                             (=fn [~bindingf]
                                  (let [~'*state* (set-cont ~'*state* old-cont#)]
                                    ~@body)))]
     ~bindingv))

  ; TODO not core macros
(defmacro =letone
  [[bindingf bindingv] & body]
  `(=bindone [~bindingf (=return ~bindingv)] ~@body))

(defmacro =tailcall
  [& body]
  `(=letone [~'_ nil] ~@body))

; TODO tailreturn

(defn save-helper [saved-stack-sym saved-snap-sym exprs]
  (if-let [expr (first exprs)]
    `(reify Snapshot
       (get-state [~'_] ~'*state*)
       (restart! [self# ~'*state*]
         (let [~'*state* (-> ~'*state*
                           (set-snapshot ~(save-helper
                                              saved-stack-sym saved-snap-sym
                                              (rest exprs)))
                           (unsnap-stack ~saved-stack-sym))]
           ~expr)))
    saved-snap-sym))

(defmacro =save-exprs! [etbody & exprs]
  (let [saved-sym (gensym "stack-snap")
        snap-sym (gensym "snap")]
    `(let [~saved-sym (snap-stack ~'*state*)
           ~snap-sym (get-snapshot ~'*state*)
           snap# ~(save-helper saved-sym snap-sym exprs)
           ~'*state* (set-snapshot ~'*state* snap#)]
       ~etbody)))

; TODO retry that doesn't consume stack
(defmacro =retry
  []
  ; TODO don't need to pass state twice?
  `(if-let [s# (get-snapshot ~'*state*)]
     (restart! s# ~'*state*)
     (=return nil)))

; TODO save-values!, ambv

; Extended macros

; TODO =amb

(defmacro =amb
  ([] `(=retry))
  ([body] body)
  ([body & bodies]
   `(=save-exprs! ~body ~@bodies)))

(defmacro =ambv
  [& bodies]
  `(=amb ~@(map (fn [x] `(=return ~x)) bodies)))

(defn iterator-snapshot [^Iterator iterator old-state]
  (let [old-stack (snap-stack old-state)
        old-snap (get-snapshot old-state)]
    (reify Snapshot
      (get-state [_] old-state)
      (restart! [self *state*]
        (let [*state* (unsnap-stack *state* old-stack)]
          (if (.hasNext iterator)
            (=return (.next iterator))
            ; when iterator is exhausted, remove us from the stack
            ; and restore the previous snapshot
            (let [*state* (set-snapshot old-state old-snap)]
              (=retry))))))))

(defmacro =amb-iterate [iterable]
  `(let [i# (.iterator ~(vary-meta iterable assoc :tag `Iterable))
         newsnap# (iterator-snapshot i# ~'*state*)
         ~'*state* (set-snapshot ~'*state* newsnap#)]
     (restart! newsnap# ~'*state*)))

(defmacro =bind [bindings & body]
  "Like let, but the binding vals may themselves be Underflow fns,
  and the continuation is saved as each binding executes."
  (if (empty? bindings)
    `(do ~@body)
    (let [tvar (first bindings)
          tval (second bindings)]
      (if (nil? tval)
        (throw (IllegalArgumentException.
                 "=bind requires an even number of forms in binding vector"))
        `(=bindone [~tvar ~tval]
                   (=bind [~@(rest (rest bindings))] ~@body))))))

(defmacro =let [bindings & body]
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
     (defmacro ~(symbol (str "=" sym)) [arg#] `(~'~sym ~~''*state* ~arg#))))

(defmacro =defn [sym [arg] & body]
  `(do
     (=declare ~sym)
     (def ~sym (=fn [~arg] ~@body))))

; Underflow

(defmacro =underflow [& body]
  `(let [state# (new-state nil)
         sfn# (reify Snapshot
                (get-state [~'_] state#)
                (restart! [~'_ ~'*state*]
                  (let [~'*state* (set-cont ~'*state* nil)]
                    ~@body)))]
     (safe-backtracker-rval (execute state# state# sfn#))))

; TODO this is type-specific
(deftype UnderflowIterator [^:unsynchronized-mutable nextv]
  Iterator
  (hasNext [_]
    (if (next-snap nextv) true false))
  ; Needs to be called once by underflow-iterator, to set the first nextv
  (next [self]
    ; Make sure this nextv didn't come from the terminal snapshot
    (if (.hasNext self)
      (let [r nextv
            next-snap (next-snap r)
            state (set-snapshot (get-state next-snap) next-snap)]
        (set! nextv (execute state state next-snap))
        (safe-backtracker-rval r))
      (throw (java.util.NoSuchElementException.))))
  (remove [_]
    (throw (UnsupportedOperationException.))))

; Used to gracefully terminate at the end of a computation, by returning nil.
(defn terminal-snapshot [state]
  ; TODO shouldn't need to pass in state--minor slowdown
  (reify Snapshot
    (get-state [self] state)
    (restart! [self *state*]
      (let [*state* (-> *state*
                      (set-snapshot nil)
                      (unsnap-stack nil))]
        (=return nil)))))

(defn do-underflow-iterator [snap]
  (let [state (new-state nil)
        state (set-snapshot state (terminal-snapshot state))]
    (UnderflowIterator. (execute state state snap))))

; TODO prefix with =
; TODO combine with underflow
(defmacro underflow-iterator [& body]
  `(let [sfn# (reify Snapshot
                (get-state [~'_] nil)
                (restart! [~'_ ~'*state*]
                  (let [~'*state* (set-cont ~'*state* nil)]
                    ~@body)))]
     (do-underflow-iterator sfn#)))

(defmacro underflow-seq [& body]
  ; TODO check thread safety of this
  `(iterator-seq (underflow-iterator ~@body)))
