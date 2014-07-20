(ns underflow.core
  "Fast continuation-based nondeterministic fns."
  (import java.util.Iterator java.lang.Iterable))

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

(defprotocol Harness
  "A harness for monadic execution. You can choose between fast or safe
  harnesses. We recommend you do not manipulate harnesses directly."
  ; Gets and sets the local value for the continuation,
  ; which is the (a -> mb) value in do-notation binds.
  (get-cont [self])
  (set-cont [self c])
  ; Boxes and unboxes return values. In the fast version, these are noops.
  (return [self r])
  (unreturn [self r])
  ; Get and set the monadic typeclass instance.
  (get-minstance [m])
  (set-minstance [self m])
  ; Run the current continuation in the harness, passing in rval.
  (execute [self rval]))

(defprotocol BacktrackingState
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
(deftype SafeState [cont m ^:unsynchronized-mutable obsolete]
  Harness
  (get-cont [_]
    (check-obsolete obsolete)
    cont)
  (set-cont [_ c]
    (set! obsolete true)
    (SafeState. c m nil))
  (return [self r]
    (if-let [cont (get-cont self)]
      (SafeStateRvalImpl. cont self r)
      (SafeBacktrackerImpl. r m)))
  (unreturn [_ r]
    (safe-backtracker-rval r))
  (set-minstance [_ s]
    (set! obsolete true)
    (SafeState. cont s nil))
  (get-minstance [_]
    (check-obsolete obsolete)
    m)
  (execute [self rval]
    (if (instance? underflow.core.SafeStateRval rval)
      (recur (safe-state-rval-execute rval))
      rval))
  BacktrackingState
  (state-from [self r]
    (set-minstance self (next-snap r))))

; All snapshots are only run once. That's the rule.
; TODO snapshot cloning
(deftype FastBacktrackingState [^:unsynchronized-mutable cont
                                ^:unsynchronized-mutable m]
  Harness
  (set-cont [self c] (set! cont c) self) 
  (get-cont [_] cont)
  (return [_ r] r)
  (unreturn [_ r] r)
  (set-minstance [self s] (set! m s) self)
  (get-minstance [_] m)
  (execute [self rval]
    (if cont
      (recur (cont self rval))
      rval))
  BacktrackingState
  (state-from [self _] self))

(deftype UnderflowIterator [^:unsynchronized-mutable state
                            ^:unsynchronized-mutable nextv]
  Iterator
  (hasNext [_]
    (if (get-minstance state) true false))
  ; Needs to be called once by underflow-iterator, to set the first nextv
  (next [self]
    ; Make sure this nextv didn't come from the terminal snapshot
    (if-let [next-snap (get-minstance state)]
      ; Get the next nextv (we're always one step ahead)
      (let [r (execute state (restart! next-snap state))
            oldv nextv]
        (set! state (state-from state r))
        (set! nextv (unreturn state r))
        oldv)
      (throw (java.util.NoSuchElementException.))))
  (remove [_]
    (throw (UnsupportedOperationException.))))

(defn safe-harness [] (SafeState. nil nil nil))
(defn fast-harness [] (FastBacktrackingState. nil nil))

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
                           (set-minstance ~(save-helper
                                              saved-stack-sym saved-snap-sym
                                              (rest exprs)))
                           (set-cont ~saved-stack-sym))]
           ~expr)))
    saved-snap-sym))

(defmacro =save-exprs! [etbody & exprs]
  (let [saved-sym (gensym "stack-snap")
        snap-sym (gensym "snap")]
    `(let [~saved-sym (get-cont ~'*state*)
           ~snap-sym (get-minstance ~'*state*)
           snap# ~(save-helper saved-sym snap-sym exprs)
           ~'*state* (set-minstance ~'*state* snap#)]
       ~etbody)))

; TODO retry that doesn't consume stack
(defmacro =retry
  []
  ; TODO don't need to pass state twice?
  `(if-let [s# (get-minstance ~'*state*)]
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
        old-snap (get-minstance old-state)]
    (reify Snapshot
      (restart! [self *state*]
        (let [*state* (set-cont *state* old-stack)]
          (if (.hasNext iterator)
            (=return (.next iterator))
            ; when iterator is exhausted, remove us from the stack
            ; and restore the previous snapshot
            (let [*state* (set-minstance old-state old-snap)]
              (=retry))))))))

(defmacro =amb-iterate [iterable]
  `(let [i# (.iterator ~(vary-meta iterable assoc :tag `Iterable))
         newsnap# (iterator-snapshot i# ~'*state*)
         ~'*state* (set-minstance ~'*state* newsnap#)]
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

(defmacro underflow
  [state & body]
  `(let [~'*state* ~state
         rval# (do ~@body)]
     (unreturn ~'*state* (execute ~'*state* rval#))))

; Used to gracefully terminate at the end of a computation, by returning nil.
(def terminal-snapshot
  (reify Snapshot
    (restart! [self *state*]
      (let [*state* (-> *state*
                      (set-minstance nil)
                      (set-cont nil))]
        (=return nil)))))

(defn do-underflow-iterator [state snap]
  (let [state (set-minstance state snap)
        iterator (UnderflowIterator. state nil)]
    ; Must get next and throw away the first value
    (.next iterator)
    iterator))

(defmacro underflow-iterator [state & body]
  `(let [sfn# (reify Snapshot
                (restart! [~'_ ~'*state*]
                  (let [~'*state* (set-minstance ~'*state* terminal-snapshot)]
                    ~@body)))]
     (do-underflow-iterator ~state sfn#)))

(defmacro underflow-seq [state & body]
  ; TODO check thread safety of this
  `(iterator-seq (underflow-iterator ~state ~@body)))
