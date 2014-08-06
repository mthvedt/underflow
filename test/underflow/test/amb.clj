(ns underflow.test.amb
  (use clojure.test underflow.core underflow.amb underflow.test.core))

(defmacro test-amb [expected & body]
  `(let [expected# ~expected
         testf# (fn [harness#] (vec (amb-seq harness# ~@body)))]
     (is (= ~expected (testf# (safe-harness))))
     (is (= ~expected (testf# (fast-harness))))))

(def mytree [[[1 2] 3] [[4 5] [6 7]]])

(=defn amb-crawl [tree]
       (if (coll? tree)
         (if (seq tree)
           (=amb (=amb-crawl (first tree)) (=amb-crawl (rest tree)))
           (=retry))
         (=return tree)))

(=defn iterate-crawl [tree]
  (if (coll? tree)
    (=bind [x (=amb-iterate tree)]
           (=iterate-crawl x))
    (=return tree)))

(=defn fast-iterate-crawl [tree]
  (if (coll? tree)
    (=bind [x (=>amb-iterate tree)]
           (=fast-iterate-crawl x))
    (=>return tree)))

(deftest test-amb-crawl-one
  (test-underflow 1 (=amb-crawl mytree)))

(deftest test-amb-crawl
         (test-amb [1 2 3 4 5 6 7] (=amb-crawl mytree))
         (test-amb [1 2 3 4 5 6 7] (=iterate-crawl mytree))
         (test-amb [1 2 3 4 5 6 7] (=fast-iterate-crawl mytree)))

(def multi-result [[1 4] [1 5] [1 6] [2 4] [2 5] [2 6] [3 4] [3 5] [3 6]])

(=defn multi-amb-test-lift [_]
       (=bind [node1 (=amblift 1 2 3)
               node2 (=amblift 4 5 6)]
              (=return [node1 node2])))

(=defn multi-crawl [_]
       (=bind [node1 (=amb-crawl [1 [2 3]])
               node2 (=amb-crawl [[4 5] 6])]
              (=return [node1 node2])))

(=defn multi-crawl-iterate [_]
       (=bind [node1 (=iterate-crawl [1 [2 3]])
               node2 (=iterate-crawl [[4 5] 6])]
              (=return [node1 node2])))

(=defn multi-crawl-fast [_]
       (=bind [node1 (=fast-iterate-crawl [1 [2 3]])
               node2 (=fast-iterate-crawl [[4 5] 6])]
              (=return [node1 node2])))

(deftest test-more-dft-continuations
  (test-amb multi-result (=multi-crawl nil))
  (test-amb multi-result (=multi-crawl-iterate nil))
  (test-amb multi-result (=multi-crawl-fast nil)))

(deftest test-empty-amb
  (test-amb [] (=amb))
  (test-amb [] (=amblift))
  (test-amb [] (=amb-iterate []))
  (test-amb [] (=>amb-iterate [])))
