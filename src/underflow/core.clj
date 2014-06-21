(ns underflow.core
  "Fast continuation-based nondeterministic fns."
  (:refer-clojure))

(defprotocol StateFn
  (call [f stack v]))

; TODO generify, definterface, interface
; Continuations should be immutable objects.
; A framed computation is a fn + a stack. Framed computations are the
; starting points. A framed computation must have save + restore.
; TODO a frame should just be a protocol--it shouldn't have save and restore baked in.
; TODO how to handle error?
; TODO instead of continuation errors, could have a special 'end' framed computation.
; each framed computation can return a lazy-seq? or maybe be an iterator?
(defprotocol State
  ; First task is to make basic underflow only understand push/pop.
  ; The wrapping underflow-seq can deal with save and restore.
  (push! [self f] "Push a continuation frame.")
  (pop! [self] "Pop a continuation frame.")
  #_(error [self] "Set the error flag.")
  #_(error? [self] "Returns the error flag.")
  #_(save! [self f] "Save a copy of the current continuation.")
  #_(continue! [self]
             "Unset the error flag and return to the previously saved continuation."))

(defprotocol ContinuingState
  (get-snapshot [self])
  (set-snapshot [self s])
  ; TODO instead use 'copy'
  (snapshot [self])
  (restore [self snapshot]))

(defprotocol Snapshot
  (restart! [self state]))

; TODO first test this
(deftype ContinuingStateImpl [continuation-stack snapshot]
  State
  ; A slow stack for now
  (push! [_ cf]
    ;(prn "Pushing" cf)
    (swap! continuation-stack conj cf))
  (pop! [_]
    ;(prn "Popping" @continuation-stack)
    (when-let [r (first @continuation-stack)] (swap! continuation-stack rest) r))
  ContinuingState
  (set-snapshot [_ s]
    (reset! snapshot s))
  (get-snapshot [_] @snapshot)
  ; TODO slow
  (snapshot [_] [@continuation-stack @snapshot])
  (restore [_ [cs s]]
    (reset! continuation-stack cs)
    (reset! snapshot s)))

(defn new-state [] (ContinuingStateImpl. (atom '()) (atom nil)))

; Core macros

(defmacro =tailrecur
  "Execute body without consuming stack.

  This generally isn't neccesary except to save stack space."
  [f & body]
  `(do
     (push! ~'*state* ~f)
     ~@body))

(defmacro =fn
  "Turn a fn of one argument into a state-passing fn."
  [[arg] & body]
  `(reify StateFn
     (call [_ ~'*state* ~arg] ~@body)))

(defmacro =letone
  "Lowest-level state-passing macro. Executes bindingv,
  and pushes onto the stack a state-passing fn accepting bindingf and executing body.
  Both bindingv and body are in tail-position."
  [[bindingf bindingv] & body]
  `(do
     (push! ~'*state* (=fn [~bindingf] ~@body))
     ~bindingv))

; Core macros for saving and restoring

(defmacro =save [rval-body & closure-body]
  `(let [state# (snapshot ~'*state*)
         snap# (reify Snapshot
                 (restart! [self# ~'*state*]
                   ;(prn "restarting saved" self#)
                   (restore ~'*state* state#)
                   ~@closure-body))]
     (set-snapshot ~'*state* snap#)
     ~rval-body))

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

(defmacro =defn [sym [arg] & body]
  `(do
     (declare ~sym)
     (defmacro ~(symbol (str "=" sym)) [arg#] `(call ~'~sym ~~''*state* ~arg#))
     (def ~sym (=fn [~arg] ~@body))))

; Underflow

(defn do-underflow
  [state start-value]
  (if-let [sfn (pop! state)]
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

(defn do-underflow-seq [snapshot]
  (let [state (new-state)
        ; TODO initial snapshot
        ; TODO s/restart!/continue
        ; TODO don't do the first computation until needed
        ; TODO test empty seq
        step (fn step [snapshot]
               (lazy-seq
                 (let [v (restart! snapshot state)
                       ;_ (prn "restarting" v)
                       v (do-underflow state v)]
                       ;_ (prn "returned" v)]
                   (when-let [c (get-snapshot state)]
                     ;(prn "got snapshot" c)
                     ; Only cons v if it's not the value from the terminal snapshot
                     (cons v (step c))))))]
    (step snapshot)))

(def terminal-snapshot
  (reify Snapshot
    (restart! [self state]
      ;(prn "terminal snapshot" self)
      ; TODO this is impl dependent
      (restore state [[] nil])
      ;(set-snapshot state nil)
      nil)))

(defmacro initial-snapshot [& body]
  `(reify Snapshot
     (restart! [self# ~'*state*]
       ;(prn "initial snapshot" self#)
       (set-snapshot ~'*state* terminal-snapshot)
       ~@body)))

(defmacro underflow-seq [& body]
  `(do-underflow-seq (initial-snapshot ~@body)))
