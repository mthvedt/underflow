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
; perf penalty. Any underflow operations should require no more than one polymorphic
; dispatch. In some places, we lean on the JVM's ability to infer static bindings,
; and hence use types containing closures.

(defprotocol State
  (get-cont [self])
  (set-cont [self c])
  (return [self r])
  (unreturn [self r])
  (execute [self]))

(defprotocol BacktrackingState
  (get-snapshot [self])
  (set-snapshot [self s])
  (state-from [self r]))

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
  (return [self r]
    (if-let [cont (get-cont self)]
      (SafeStateRvalImpl. cont self r)
      (SafeBacktrackerImpl. r statev)))
  (unreturn [_ r]
    (safe-backtracker-rval r))
  ; TODO peformance test this.
  ; The snapfn should be stack allocated in most circumstances.
  (execute [self]
    ; A snapfn is basically a Haskell cont monad--it deposits a value
    ; into a continuation.
    (loop [v (restart! statev self)]
      (if (instance? underflow.core.SafeStateRval v)
        (recur (safe-state-rval-execute v))
        v)))
  BacktrackingState
  (state-from [self r]
    (set-snapshot self (next-snap r)))
  (set-snapshot [_ s]
    (set! obsolete true)
    (SafeState. cont s nil))
  (get-snapshot [_]
    (check-obsolete obsolete)
    statev))

; All snapshots are only run once. That's the rule.
; TODO snapshot cloning
(deftype FastBacktrackingState [^:unsynchronized-mutable cont
                                ^:unsynchronized-mutable snap]
  State
  (set-cont [self c] (set! cont c) self) 
  (get-cont [_] cont)
  (return [_ r] r)
  (unreturn [_ r] r)
  (execute [self]
    (loop [v (restart! snap self)]
      (if cont
        (recur (cont self v))
        v)))
  BacktrackingState
  (state-from [self _] self)
  (set-snapshot [self s] (set! snap s) self)
  (get-snapshot [_] snap))

; TODO rearrange
(deftype UnderflowIterator [^:unsynchronized-mutable state
                            ^:unsynchronized-mutable nextv]
  Iterator
  (hasNext [_]
    (if (get-snapshot state) true false))
  ; Needs to be called once by underflow-iterator, to set the first nextv
  (next [self]
    ; Make sure this nextv didn't come from the terminal snapshot
    (if (.hasNext self)
      (let [r (execute state)
            oldv nextv]
        ; Get the next nextv (we're always one step ahead)
        (set! state (state-from state r))
        (set! nextv (unreturn state r))
        oldv)
      (throw (java.util.NoSuchElementException.))))
  (remove [_]
    (throw (UnsupportedOperationException.))))

;(defn new-state [] (SafeState. nil nil nil))
(defn new-state [] (FastBacktrackingState. nil nil))

; Core macros

(defmacro =fn
  "Turn a fn of one argument into a state-passing fn."
  [[arg] & body]
  `(fn [~'*state* ~arg] ~@body))

(defmacro =return
  [& body]
  `(return ~'*state* (do ~@body)))

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
         (let [~'*state* (-> ~'*state*
                           (set-snapshot ~(save-helper
                                              saved-stack-sym saved-snap-sym
                                              (rest exprs)))
                           (set-cont ~saved-stack-sym))]
           ~expr)))
    saved-snap-sym))

(defmacro =save-exprs! [etbody & exprs]
  (let [saved-sym (gensym "stack-snap")
        snap-sym (gensym "snap")]
    `(let [~saved-sym (get-cont ~'*state*)
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

; Extended macros

(defmacro =amb
  ([] `(=retry))
  ([body] body)
  ([body & bodies]
   `(=save-exprs! ~body ~@bodies)))

(defmacro =ambv
  [& bodies]
  `(=amb ~@(map (fn [x] `(=return ~x)) bodies)))

; TODO there's mutability here
(defn iterator-snapshot [^Iterator iterator old-state]
  (let [old-stack (get-cont old-state)
        old-snap (get-snapshot old-state)]
    (reify Snapshot
      (restart! [self *state*]
        (let [*state* (set-cont *state* old-stack)]
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

(defmacro underflow [& body]
  ; TODO should only require state monad
  `(let [state# (new-state)
         sfn# (reify Snapshot
                (restart! [~'_ ~'*state*]
                  (let [~'*state* (set-cont ~'*state* nil)]
                    ~@body)))
         state# (set-snapshot state# sfn#)]
     (unreturn state# (execute state#))))

; Used to gracefully terminate at the end of a computation, by returning nil.
(defn terminal-snapshot [state]
  ; TODO shouldn't need to pass in state--minor slowdown
  (reify Snapshot
    (restart! [self *state*]
      (let [*state* (-> *state*
                      (set-snapshot nil)
                      (set-cont nil))]
        (=return nil)))))

(defn do-underflow-iterator [state snap]
  (let [state (set-snapshot state snap)
        iterator (UnderflowIterator. state nil)]
    ; Must get next and throw away the first value
    (.next iterator)
    iterator))

; TODO prefix with =
; TODO combine with underflow
(defmacro underflow-iterator [& body]
  `(let [state# (new-state)
         sfn# (reify Snapshot
                (restart! [~'_ ~'*state*]
                  (let [~'*state* (set-snapshot ~'*state*
                                                (terminal-snapshot ~'*state*))]
                    ~@body)))]
     (do-underflow-iterator state# sfn#)))

; TODO =
(defmacro underflow-seq [& body]
  ; TODO check thread safety of this
  `(iterator-seq (underflow-iterator ~@body)))
