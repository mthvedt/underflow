(ns underflow.amb
  "Fast continuation-based nondeterministic fns."
  (import java.util.Iterator java.lang.Iterable
          (underflow.java Harness Continuation))
  (use underflow.core))

; All snapshots are only run once. That's the rule.
; TODO snapshot cloning

; TODO something preventing optimization here
(deftype UnderflowIterator [^:unsynchronized-mutable ^Harness state
                            ^:unsynchronized-mutable nextv]
  Iterator
  (hasNext [_]
    (if (.getDict state) true false))
  ; Needs to be called once by underflow-iterator, to set the first nextv
  (next [self]
    ; Make sure this nextv didn't come from the terminal snapshot
    (if-let [next-snap (.getDict state)]
      ; Get the next nextv (we're always one step ahead)
      (let [r (.execute state (.call next-snap state nil))
            oldv nextv]
        (set! state (.reharness state r))
        (set! nextv (.extract state r))
        oldv)
      (throw (java.util.NoSuchElementException.))))
  (remove [_]
    (throw (UnsupportedOperationException.))))

(defmacro snapshot [& body]
  `(reify Continuation
     (call [self# ~'*state* ~'_] ~@body)))

(defn save-helper [saved-stack-sym saved-snap-sym exprs]
  (if-let [expr (first exprs)]
    `(snapshot
       (let [~'*state* (-> ~'*state*
                         (.setDict ~(save-helper
                                      saved-stack-sym saved-snap-sym
                                      (rest exprs)))
                         (.setCont ~saved-stack-sym))]
         ~expr))
    saved-snap-sym))

(defmacro =save-exprs! [etbody & exprs]
  (let [saved-sym (gensym "stack-snap")
        snap-sym (gensym "snap")]
    `(let [~saved-sym (.getCont ~'*state*)
           ~snap-sym (.getDict ~'*state*)
           snap# ~(save-helper saved-sym snap-sym exprs)
           ~'*state* (.setDict ~'*state* snap#)]
       ~etbody)))

(defmacro =retry
  []
  `(if-let [s# (.getDict ~'*state*)]
     ; Major slowdown, but means failure doesn't consume a stack frame
     (=with-cont s# (=return nil))
     (=return nil)))

(defmacro =>retry
  []
  `(if-let [s# (.getDict ~'*state*)]
     ; TODO is this a regression?
     (=with-cont s# (=>return nil))
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
  [^Iterator iterator ^Harness old-state]
  (let [old-cont (.getCont old-state)
        old-snap (.getDict old-state)]
    (snapshot
      (let [*state* (.setCont *state* old-cont)]
        (if (.hasNext iterator)
          (=return (.next iterator))
          ; when iterator is exhausted, remove us from the stack
          ; and restore the previous snapshot
          (let [*state* (.setDict old-state old-snap)]
            (=retry)))))))

(defn push-iterator-snapshot
  [^Iterator iterator ^Harness old-state]
  (let [old-cont (.getCont old-state)
        old-snap (.getDict old-state)]
    (snapshot 
      (let [*state* (.setCont *state* old-cont)]
        (if (.hasNext iterator)
          (=>return (.next iterator))
          ; when iterator is exhausted, remove us from the stack
          ; and restore the previous snapshot
          (let [*state* (.setDict old-state old-snap)]
            (=>retry)))))))

(defmacro =amb-iterate-experiment
  [iterable-form]
  (let [isym (gensym "iterable")]
    `(let [~isym ~iterable-form
           old-cont# (.getCont ~'*state*)
           old-snap# (.getDict ~'*state*)
           iterator# (.iterator ~(tag isym `Iterable))]
       (if (.hasNext iterator#)
         (let [newsnap# (snapshot
                          (let [~'*state* (.setCont ~'*state* old-cont#)]
                            (if (.hasNext iterator#)
                              (=>return (.next iterator#))
                              ; when iterator is exhausted, remove us from the stack
                              ; and restore the previous snapshot
                              (let [~'*state* (.setDict ~'*state* old-snap#)]
                                (=>retry)))))
               ~'*state* (.setDict ~'*state* newsnap#)]
           (=>return (.next iterator#)))
         (=>retry)))))

(defmacro =amb-iterate
  [iterable]
  `(let [i# (.iterator ~(tag iterable `Iterable))]
     (if (.hasNext i#)
       (let [newsnap# (iterator-snapshot i# ~'*state*)
             ~'*state* (.setDict ~'*state* newsnap#)]
         (=return (.next i#)))
       (=retry))))

(defmacro =>amb-iterate
  [iterable]
  `(let [i# (.iterator ~(tag iterable `Iterable))]
     (if (.hasNext i#)
       (let [newsnap# (push-iterator-snapshot i# ~'*state*)
             ~'*state* (.setDict ~'*state* newsnap#)]
         (=>return (.next i#)))
       (=>retry))))

; Used to gracefully terminate at the end of a computation by returning nil.
(deftype TerminalSnapshot []
  Continuation
  (call [self *state* _]
    (let [*state* (-> *state*
                    (.setDict nil)
                    (.setCont nil))]
      (=return nil))))
(def terminal-snapshot (TerminalSnapshot.))

(defn do-underflow-iterator [^Harness state ^Continuation snap]
  (let [state (.setDict state snap)
        iterator (UnderflowIterator. state nil)]
    ; Must get next and throw away the first value
    (.next iterator)
    iterator))

(defmacro underflow-iterator [state & body]
  `(let [sfn# (snapshot
                (let [~'*state* (.setDict ~'*state* terminal-snapshot)]
                  ~@body))]
     (do-underflow-iterator ~state sfn#)))

(defmacro underflow-seq [state & body]
  ; TODO check thread safety of this
  `(iterator-seq (underflow-iterator ~state ~@body)))
