(ns underflow.amb
  "Fast continuation-based nondeterministic fns."
  (import java.util.Iterator java.lang.Iterable
          (underflow.java Harness Continuation))
  (use underflow.core))

; All snapshots are only run once. That's the rule.
; TODO snapshot cloning

(defmacro snapshot [& body]
  `(reify Continuation
     (call [self# ~'*state* ~'_] ~@body)))

(defmacro =retry
  []
  (let [ss (tag (gensym "snap") `Continuation)]
    `(if-let [~ss (.getDict ~'*state*)]
       ; Major slowdown, but means failure doesn't consume a stack frame
       (=with-cont ~ss (=return nil))
       (=return nil))))

(defmacro =>retry
  []
  (let [ss (tag (gensym "snap") `Continuation)]
    `(if-let [~ss (.getDict ~'*state*)]
       ; TODO is this a regression?
       (=with-cont ~ss (=>return nil))
       (=>return nil))))

(defn amb-helper [saved-stack-sym saved-snap-sym exprs]
  (if-let [expr (first exprs)]
    `(snapshot
       (let [~'*state* (-> ~'*state*
                         (.setDict ~(amb-helper
                                      saved-stack-sym saved-snap-sym
                                      (rest exprs)))
                         (.setCont ~saved-stack-sym))]
         ~expr))
    (tag saved-snap-sym `Snapshot)))

(defmacro =amb
  ([] `(=>retry))
  ([body] body)
  ([body & bodies]
   (let [saved-sym (gensym "stack-snap")
         snap-sym (gensym "snap")]
     `(let [~saved-sym (.getCont ~'*state*)
            ~snap-sym (.getDict ~'*state*)
            snap# ~(amb-helper saved-sym snap-sym bodies)
            ~'*state* (.setDict ~'*state* snap#)]
        ~body))))

(defmacro =amblift
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

(defmacro =amb-iterate
  [iterable]
  (let [ibs (tag (gensym "iterable") `Iterable)]
    `(let [~ibs ~iterable
           i# (.iterator ~ibs)]
       (if (.hasNext i#)
         (let [newsnap# (iterator-snapshot i# ~'*state*)
               ~'*state* (.setDict ~'*state* newsnap#)]
           (=return (.next i#)))
         (=retry)))))

(defmacro =>amb-iterate
  [iterable-form]
  (let [isym (tag (gensym "iterable") `Iterable)]
    `(let [~isym ~iterable-form
           old-cont# (.getCont ~'*state*)
           old-snap# (.getDict ~'*state*)
           iterator# (.iterator ~isym)]
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

; Used to gracefully terminate at the end of a computation by returning nil.
(deftype TerminalSnapshot []
  Continuation
  (call [self *state* _]
    (let [*state* (-> *state*
                    (.setDict nil)
                    (.setCont nil))]
      (=return nil))))
(def terminal-snapshot (TerminalSnapshot.))

; TODO something is preventing optimizing the dispatches in this class.
(deftype AmbIterator [^:unsynchronized-mutable ^Harness state
                      ^:unsynchronized-mutable nextv]
  Iterator
  (hasNext [_]
    (if (.getDict state) true false))
  ; Needs to be called once to set the first nextv
  (next [self]
    ; Make sure this nextv didn't come from the terminal snapshot
    (if-let [^Continuation next-snap (.getDict state)]
      ; Get the next nextv (we're always one step ahead)
      (let [r (.execute state (.call next-snap state nil))
            oldv nextv]
        (set! state (.reharness state r))
        (set! nextv (.extract state r))
        oldv)
      (throw (java.util.NoSuchElementException.))))
  (remove [_]
    (throw (UnsupportedOperationException.))))

(defn do-amb-iterator [^Harness state ^Continuation snap]
  (let [state (.setDict state snap)
        iterator (AmbIterator. state nil)]
    ; Must get next and throw away the first value
    (.next iterator)
    iterator))

(defmacro amb-iterator [state & body]
  `(let [sfn# (snapshot
                (let [~'*state* (.setDict ~'*state* terminal-snapshot)]
                  ~@body))]
     (do-amb-iterator ~state sfn#)))

(defn do-amb-seq [^Harness state ^Continuation snap]
  ((fn stepf [state]
     (lazy-seq
       (let [r (.execute state (.call (.getDict state) state nil))
             state (.reharness state r)]
         (if-let [^Continuation next-snap (.getDict state)]
           ; Make sure this didn't come from the terminal snapshot
           (cons
             (.extract state r)
             (stepf state))))))
     (.setDict state snap)))

(defmacro amb-seq [state & body]
  `(let [sfn# (snapshot
                (let [~'*state* (.setDict ~'*state* terminal-snapshot)]
                  ~@body))]
     (do-amb-seq ~state sfn#)))
