(ns underflow.amb
  "Fast continuation-based nondeterministic fns."
  (import java.util.Iterator java.lang.Iterable)
  (use underflow.core))

; All snapshots are only run once. That's the rule.
; TODO snapshot cloning

; TODO something preventing optimization here
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
      (let [r (execute state (call-cont next-snap state nil))
            oldv nextv]
        (set! state (reharness state r))
        (set! nextv (extract state r))
        oldv)
      (throw (java.util.NoSuchElementException.))))
  (remove [_]
    (throw (UnsupportedOperationException.))))

(defmacro snapshot [& body]
  `(reify Continuation
     (call-cont [self# ~'*state* ~'_] ~@body)))

(defn save-helper [saved-stack-sym saved-snap-sym exprs]
  (if-let [expr (first exprs)]
    `(snapshot
       (let [~'*state* (-> ~'*state*
                         (set-dict ~(save-helper
                                      saved-stack-sym saved-snap-sym
                                      (rest exprs)))
                         (set-cont ~saved-stack-sym))]
         ~expr))
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
     ;TODO this is a regression. why?
     (=with-cont s# (=return nil))
     (=return nil)))

(defmacro =>retry
  []
  `(if-let [s# (get-dict ~'*state*)]
     ; TODO is this a regression?
     ;(=with-cont s# (=>return nil))
     (call-cont s# ~'*state* nil)
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
  (let [old-cont (get-cont old-state)
        old-snap (get-dict old-state)]
    (snapshot
      (let [*state* (set-cont *state* old-cont)]
        (if (.hasNext iterator)
          (=return (.next iterator))
          ; when iterator is exhausted, remove us from the stack
          ; and restore the previous snapshot
          (let [*state* (set-dict old-state old-snap)]
            (=retry)))))))

(defn push-iterator-snapshot
  [^Iterator iterator old-state]
  (let [old-cont (get-cont old-state)
        old-snap (get-dict old-state)]
    (snapshot 
      (let [*state* (set-cont *state* old-cont)]
        (if (.hasNext iterator)
          (=>return (.next iterator))
          ; when iterator is exhausted, remove us from the stack
          ; and restore the previous snapshot
          (let [*state* (set-dict old-state old-snap)]
            (=>retry)))))))

(defmacro =amb-iterate-experiment
  [iterable-form]
  (let [isym (gensym "iterable")]
    `(let [~isym ~iterable-form
           old-cont# (get-cont ~'*state*)
           old-snap# (get-dict ~'*state*)
           iterator# (.iterator ~(vary-meta isym assoc :tag `Iterable))]
       (if (.hasNext iterator#)
         (let [newsnap# (snapshot
                          (let [~'*state* (set-cont ~'*state* old-cont#)]
                            (if (.hasNext iterator#)
                              (=>return (.next iterator#))
                              ; when iterator is exhausted, remove us from the stack
                              ; and restore the previous snapshot
                              (let [~'*state* (set-dict ~'*state* old-snap#)]
                                (=>retry)))))
               ~'*state* (set-dict ~'*state* newsnap#)]
           (=>return (.next iterator#)))
         (=>retry)))))

(defmacro =amb-iterate
  [iterable]
  `(let [i# (.iterator ~(vary-meta iterable assoc :tag `Iterable))]
     (if (.hasNext i#)
       (let [newsnap# (iterator-snapshot i# ~'*state*)
             ~'*state* (set-dict ~'*state* newsnap#)]
         (=return (.next i#)))
       (=retry))))

(defmacro =>amb-iterate
  [iterable]
  `(let [i# (.iterator ~(vary-meta iterable assoc :tag `Iterable))]
     (if (.hasNext i#)
       (let [newsnap# (push-iterator-snapshot i# ~'*state*)
             ~'*state* (set-dict ~'*state* newsnap#)]
         (=>return (.next i#)))
       (=>retry))))

; Used to gracefully terminate at the end of a computation by returning nil.
(deftype TerminalSnapshot []
  Continuation
  (call-cont [self *state* _]
    (let [*state* (-> *state*
                    (set-dict nil)
                    (set-cont nil))]
      (=return nil))))
(def terminal-snapshot (TerminalSnapshot.))

(defn do-underflow-iterator [state snap]
  (let [state (set-dict state snap)
        iterator (UnderflowIterator. state nil)]
    ; Must get next and throw away the first value
    (.next iterator)
    iterator))

(defmacro underflow-iterator [state & body]
  `(let [sfn# (snapshot
                (let [~'*state* (set-dict ~'*state* terminal-snapshot)]
                  ~@body))]
     (do-underflow-iterator ~state sfn#)))

(defmacro underflow-seq [state & body]
  ; TODO check thread safety of this
  `(iterator-seq (underflow-iterator ~state ~@body)))
