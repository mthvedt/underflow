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

; TODO the monad thing works after all. First work on the case w/o amb.

(defprotocol State
  (get-cont [self])
  (set-cont [self c])
  (return [self other-state r])
  (execute [self other-state start-value]))
  ;(snap-state [self])
  ;(unsnap-state [self state]))

(defprotocol BacktrackingState
  (get-snapshot [self])
  (set-snapshot [self s])
  (snap-stack [self])
  (unsnap-stack [self s]))

(defprotocol Snapshot
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
  (next-state [self]))
(deftype SafeBacktrackerImpl [r s]
  SafeBacktracker
  (safe-backtracker-rval [_] r)
  (next-state [_] s))

; TODO this is actually a generic stateful computation.
(deftype SafeState [cont statev]
  State
  (get-cont [_] cont)
  (set-cont [_ c] (SafeState. c statev))
  (return [self other-state r]
    ; TODO underflow identity isn't doing its thing properly.
    ; how to faster terminate execution?
    (if-let [cont (get-cont self)]
      (SafeStateRvalImpl. cont other-state r)
      (SafeBacktrackerImpl. r statev)))
  (execute [self other-state v]
    (if cont
      (loop [v (cont other-state v)]
        (if (instance? underflow.core.SafeStateRval v)
          (recur (safe-state-rval-execute v))
          (safe-backtracker-rval v))))))

(deftype MutableState [^:unsynchronized-mutable cont]
  State
  (get-cont [_] cont)
  (set-cont [self c] (set! cont c) self)
  (return [_ _ r] r)
  (execute [self other-state v]
    (if cont
      (recur other-state (cont other-state v))
      v)))

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
  (snap-stack [this] (get-cont this))
  (unsnap-stack [this c] (set-cont this c)))

;(defn new-state [] (FastBacktrackingState. (SafeState. nil) nil))
;(defn new-state [] (FastBacktrackingState. (MutableState. nil) nil))
(defn new-state [sfn] (SafeState. sfn nil))
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

; TODO retry that doesn't consume stack
(defmacro =retry []
  ; TODO don't need to pass state twice?
  `(when-let [s# (get-snapshot ~'*state*)]
     ;(prn "retrying" s#)
     (restart! s# ~'*state*)))

; TODO save-values!, ambv

; Extended macros

; TODO do we need this?
(defmacro =save! [& body]
  `(=save-exprs! (do ~@body)))

(defmacro =amb
  ([] `(=retry))
  ([body] body)
  ([body & bodies]
   `(do
      (=save-exprs! ~@bodies)
      ~body)))

(defn iterator-snapshot [^Iterator iterator old-stack old-snap]
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
         i# (.iterator ~(vary-meta iterable assoc :tag `Iterable))
         newsnap# (iterator-snapshot i# saved# snap#)]
     (if (.hasNext i#)
       (do
         (set-snapshot ~'*state* newsnap#)
         (.next i#))
       (=retry))))

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
  `(let [sfn# (=fn [~'_]
                   (let [~'*state* (set-cont ~'*state* nil)]
                     ~@body))
         state# (new-state sfn#)]
     (execute state# state# nil)))

(deftype UnderflowIterator [^:unsynchronized-mutable nextv state]
  Iterator
  (hasNext [_]
    ;(prn "Has next " (get-snapshot state))
    (if (get-snapshot state) true false))
  ; Needs to be called once by underflow-iterator, to set the first nextv
  (next [_]
    ;(prn "Nextv is " nextv " snapshot is " (get-snapshot state))
    ; Make sure this nextv didn't come from the terminal snapshot
    (if-let [snap (get-snapshot state)]
      (let [r nextv]
        (set! nextv (execute state state (restart! snap state)))
        r)
      (throw (java.util.NoSuchElementException.))))
  (remove [_]
    (throw (UnsupportedOperationException.))))

(defn do-underflow-iterator [snapshot]
  (let [state (new-state)
        _ (set-snapshot state snapshot)
        r (UnderflowIterator. nil state)]
    ; See above comments for UnderflowIterator
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
