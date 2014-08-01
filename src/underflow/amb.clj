(ns underflow.amb
  "Fast continuation-based nondeterministic fns."
  (import java.util.Iterator java.lang.Iterable)
  (use underflow.core))

; All snapshots are only run once. That's the rule.
; TODO snapshot cloning
(defprotocol Snapshot
  (run-snapshot [self state]))

(deftype UnderflowIterator [^:unsynchronized-mutable state
                            ^:unsynchronized-mutable nextv]
  Iterator
  (hasNext [_]
    (if (get-dict state) true false))
  ; Needs to be called once by underflow-iterator, to set the first nextv
  (next [self]
    ; Make sure this nextv didn't come from the terminal snapshot
    (if-let [next-snap (get-dict state)]
      ; Get the next nextv (we're always one step ahead)
      (let [r (execute state (run-snapshot next-snap state))
            oldv nextv]
        (set! state (reharness state r))
        (set! nextv (extract state r))
        oldv)
      (throw (java.util.NoSuchElementException.))))
  (remove [_]
    (throw (UnsupportedOperationException.))))

(defn save-helper [saved-stack-sym saved-snap-sym exprs]
  (if-let [expr (first exprs)]
    `(reify Snapshot
       (run-snapshot [self# ~'*state*]
         (let [~'*state* (-> ~'*state*
                           (set-dict ~(save-helper
                                              saved-stack-sym saved-snap-sym
                                              (rest exprs)))
                           (set-cont ~saved-stack-sym))]
           ~expr)))
    saved-snap-sym))

(defmacro =save-exprs! [etbody & exprs]
  (let [saved-sym (gensym "stack-snap")
        snap-sym (gensym "snap")]
    `(let [~saved-sym (get-cont ~'*state*)
           ~snap-sym (get-dict ~'*state*)
           snap# ~(save-helper saved-sym snap-sym exprs)
           ~'*state* (set-dict ~'*state* snap#)]
       ~etbody)))

(defmacro =retry
  []
  `(if-let [s# (get-dict ~'*state*)]
     ; This trick sets the continuation to (run-snapshot ...) and returns nil
     (=letone [~'_ nil] (run-snapshot s# ~'*state*))
     (=return nil)))

(defmacro =>retry
  []
  `(if-let [s# (get-dict ~'*state*)]
     (run-snapshot s# ~'*state*)
     (=>return nil)))

; TODO nomenclature. don't use =amb/=ambv
(defmacro =amb
  ([] `(=retry))
  ([body] body)
  ([body & bodies]
   `(=save-exprs! ~body ~@bodies)))

(defmacro =ambv
  [& bodies]
  `(=amb ~@(map (fn [x] `(=return ~x)) bodies)))

; TODO there's mutability here
(defn iterator-snapshot
  [^Iterator iterator old-state]
  (let [old-stack (get-cont old-state)
        old-snap (get-dict old-state)]
    (reify Snapshot
      (run-snapshot [self *state*]
        (let [*state* (set-cont *state* old-stack)]
          (if (.hasNext iterator)
            (=return (.next iterator))
            ; when iterator is exhausted, remove us from the stack
            ; and restore the previous snapshot
            (let [*state* (set-dict old-state old-snap)]
              (=retry))))))))

(defn push-iterator-snapshot
  [^Iterator iterator old-state]
  (let [old-stack (get-cont old-state)
        old-snap (get-dict old-state)]
    (reify Snapshot
      (run-snapshot [self *state*]
        (let [*state* (set-cont *state* old-stack)]
          (if (.hasNext iterator)
            (=>return (.next iterator))
            ; when iterator is exhausted, remove us from the stack
            ; and restore the previous snapshot
            (let [*state* (set-dict old-state old-snap)]
              (=>retry))))))))

(defmacro =amb-iterate
  [iterable]
  `(let [i# (.iterator ~(vary-meta iterable assoc :tag `Iterable))
         newsnap# (iterator-snapshot i# ~'*state*)
         ~'*state* (set-dict ~'*state* newsnap#)]
     (run-snapshot newsnap# ~'*state*)))

(defmacro =>amb-iterate
  [iterable]
  `(let [i# (.iterator ~(vary-meta iterable assoc :tag `Iterable))
         newsnap# (push-iterator-snapshot i# ~'*state*)
         ~'*state* (set-dict ~'*state* newsnap#)]
     (run-snapshot newsnap# ~'*state*)))

; Used to gracefully terminate at the end of a computation, by returning nil.
(def terminal-snapshot
  (reify Snapshot
    (run-snapshot [self *state*]
      (let [*state* (-> *state*
                      (set-dict nil)
                      (set-cont nil))]
        (=return nil)))))

(defn do-underflow-iterator [state snap]
  (let [state (set-dict state snap)
        iterator (UnderflowIterator. state nil)]
    ; Must get next and throw away the first value
    (.next iterator)
    iterator))

(defmacro underflow-iterator [state & body]
  `(let [sfn# (reify Snapshot
                (run-snapshot [~'_ ~'*state*]
                  (let [~'*state* (set-dict ~'*state* terminal-snapshot)]
                    ~@body)))]
     (do-underflow-iterator ~state sfn#)))

(defmacro underflow-seq [state & body]
  ; TODO check thread safety of this
  `(iterator-seq (underflow-iterator ~state ~@body)))
