(ns underflow.core)

; DESIGN DECISIONS
; * Speed in Java comes from inlining and avoiding object construction.
; Underflow should offer both as options whenever possible.
; * Clojure is based on safety. Underflow should respect the safety/speed tradeoff.
; * Use of Underflow should be as low effort as possible, except when it interferes
; with the above two goals.

; IMPLEMENTATION DETAILS
; * Calls to the Harness are small monomorphic (or at worst dimorphic) calls,
; for maximum inlinability.
; * Most functionality is accomplished with macros reifying closures.
; This is preferred over types containing closures, to avoid the 'double dispatch'
; perf penalty. Any underflow operations should require no more than one polymorphic
; dispatch. In some places, we lean on the JVM's ability to infer static bindings,
; and hence use types containing closures.

(defprotocol Harness
  "A harness for monadic execution. You can choose between fast or safe
  harnesses. We recommend you do not manipulate harnesses directly."
  ; Harness fns are assumed to be highly inlinable.

  ; Gets and sets the local value for the continuation,
  ; which is the (a -> mb) value in do-notation binds.
  (get-cont [self])
  (set-cont [self c])
  ; Boxes and unboxes return values. In the fast version, these are noops.
  (return [self r])
  (fast-return [self r])
  (extract [self r])
  ; Get a new harness from a return value.
  (reharness [self r])
  ; Get and set the monadic typeclass instance.
  (get-dict [m])
  (set-dict [self m])
  ; Run the current continuation in the harness, passing in rval.
  (execute [self rval]))

(defprotocol Continuation
  ; TODO run-cont
  (call-cont [self harness rval]))

; Marker type
(defprotocol HarnessIntermediateValue
  (safe-state-rval-execute [self]))
; Separate deftype to ease debugging stack traces
(deftype HarnessIntermediateValueImpl [c s v]
  HarnessIntermediateValue
  (safe-state-rval-execute [self]
    (call-cont c s v)))

; TODO expose these?
(defprotocol HarnessResult
  (safe-backtracker-rval [self])
  (next-snap [self]))
(deftype HarnessResultImpl [r s]
  HarnessResult
  (safe-backtracker-rval [_] r)
  (next-snap [_] s))

(defn check-obsolete [obsolete]
  (if obsolete
    (throw (IllegalStateException. "Using old reference"))))

(deftype SafeState [cont m ^:unsynchronized-mutable obsolete]
  Harness
  (get-cont [_]
    (check-obsolete obsolete)
    cont)
  (set-cont [_ c]
    (set! obsolete true)
    (SafeState. c m nil))
  (return [self r]
    (check-obsolete obsolete)
    (if cont
      (HarnessIntermediateValueImpl. cont self r)
      (HarnessResultImpl. r m)))
  (fast-return [self r] (return self r))
  (extract [_ r]
    (safe-backtracker-rval r))
  (set-dict [_ s]
    (set! obsolete true)
    (SafeState. cont s nil))
  (get-dict [_]
    (check-obsolete obsolete)
    m)
  (execute [self rval]
    (if (instance? underflow.core.HarnessIntermediateValue rval)
      (recur (safe-state-rval-execute rval))
      rval))
  (reharness [self r]
    (set-dict self (next-snap r))))

(deftype FastBacktrackingState [^:unsynchronized-mutable cont
                                ^:unsynchronized-mutable m]
  Harness
  (set-cont [self c] (set! cont c) self) 
  (get-cont [_] cont)
  (return [_ r] r)
  (fast-return [self r] (when cont (call-cont cont self r)))
  (extract [_ r] r)
  (set-dict [self s] (set! m s) self)
  (get-dict [_] m)
  (execute [self rval]
    (if cont
      (recur (call-cont cont self rval))
      rval))
  (reharness [self _] self))

(defn safe-harness [] (SafeState. nil nil nil))
(defn fast-harness [] (FastBacktrackingState. nil nil))

; Core macros
(defmacro =fn
  "Turn a fn of one argument into a state-passing fn."
  [[arg] & body]
  `(reify Continuation
     (call-cont [~'_ ~'*state* ~arg] ~@body)))

(defmacro =named-fn
  [name [arg] & body]
  `(reify Continuation
     (call-cont [self# ~'*state* ~arg]
       ; This fn should die in escape analysis. TODO TEST
       (let [~name (fn [arg# state#] (call-cont self# arg# state#))]
         ~@body))))

(defmacro =return
  [& body]
  `(return ~'*state* (do ~@body)))

(defmacro =call
  [f arg]
  `(~f ~'*state* ~arg))

(defmacro =>return
  "Like =return, but consumes stack in the fast implementation."
  [& body]
  `(fast-return ~'*state* (do ~@body)))

(defmacro =with-cont
  "Totally overrides the old continuation. Use with caution."
  [cont & body]
  `(let [~'*state* (set-cont ~'*state* ~cont)] ~@body))

(defmacro =bindone
  [[bindingf bindingv] & body]
  `(let [old-cont# (get-cont ~'*state*)
         ~'*state* (set-cont ~'*state*
                             (=fn [~bindingf]
                                  (let [~'*state* (set-cont ~'*state* old-cont#)]
                                    ~@body)))]
     ~bindingv))

; Extended macros

(defmacro =letone
  [[bindingf bindingv] & body]
  `(=bindone [~bindingf (=return ~bindingv)] ~@body))

(defmacro =tailcall
  [& body]
  `(=letone [~'_ nil] ~@body))

(defmacro =bind [bindings & body]
  "Like let, but the binding vals may themselves be Underflow fns,
  and the continuation is saved as each binding executes."
  (if (empty? bindings)
    `(do ~@body)
    (let [tvar (first bindings)
          tval (second bindings)]
      (if-not (seq (rest bindings))
        (throw (IllegalArgumentException.
                 "=bind requires an even number of forms in binding vector"))
        `(=bindone [~tvar ~tval]
                   (=bind [~@(rest (rest bindings))] ~@body))))))

(defmacro =let [bindings & body]
  (if (empty? bindings)
    `(do ~@body)
    (let [tvar (first bindings)
          tval (second bindings)]
      (if-not (seq (rest bindings))
        (throw (IllegalArgumentException.
                 "=let requires an even number of forms in binding vector"))
        `(=letone [~tvar ~tval]
                  (=let [~@(rest (rest bindings))] ~@body))))))

(defmacro =declare [sym]
  `(do
     (declare ~sym)
     (defmacro ~(symbol (str "=" sym)) [arg#] `(call-cont ~'~sym ~~''*state* ~arg#))))

(defmacro =defn [sym [arg] & body]
  `(do
     (=declare ~sym)
     (def ~sym (=fn [~arg] ~@body))))

; Underflow

(defmacro underflow
  [state & body]
  `(let [~'*state* ~state
         rval# (do ~@body)]
     (extract ~'*state* (execute ~'*state* rval#))))
