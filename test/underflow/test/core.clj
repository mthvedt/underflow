(ns underflow.test.core
  (use clojure.test underflow.core))

(defmacro test-underflow [expected & body]
  `(let [expected# ~expected
         testf# (fn [harness#] (underflow harness# ~@body))]
     (is (= expected# (testf# (safe-harness))))
     (is (= expected# (testf# (fast-harness))))))

(=defn basic-testf [x] (=return x))

(deftest basic-test (test-underflow 1 (=basic-testf 1)))

(=declare funny-odd?)

(=defn funny-even? [x]
       (if (= x 0)
         (=return true)
         (=tailcall (=funny-odd? (dec x)))))

(=defn funny-odd? [x]
       (if (= x 0)
         (=return false)
         (=tailcall (=funny-even? (dec x)))))

(deftest test-tailrecur
  (test-underflow true (=funny-even? 10))
  (test-underflow false (=funny-even? 11))
  (test-underflow false (=funny-odd? 10))
  (test-underflow true (=funny-odd? 11)))

(=defn fib [x]
       (case x
         0 (=return 1)
         1 (=return 1)
         (=bind [a (=fib (- x 1))
                 b (=fib (- x 2))]
                (=return (+ a b)))))

; TODO test failures. premature return shouldnt be allowed
; (the below tests without return should fail in safe mode)
(deftest test-let-and-bind
  (test-underflow 6 (=let [z (+ 1 2)]
                          (=return (+ z 3))))
  (test-underflow 9 (=let [z (+ 1 2)
                           z2 (+ z 3)]
                          (=return (+ z z2))))
  (test-underflow 9 (=let [z (+ 1 2)]
                          (=let [z2 (+ z 3)]
                                (=return (+ z z2)))))
  (test-underflow 8 (=fib 5))
  (test-underflow 89 (=fib 10)))

(defmacro test-amb [expected & body]
  `(let [expected# ~expected
         testf# (fn [harness#] (vec (underflow-seq harness# ~@body)))]
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

(deftest test-amb-crawl
  (test-underflow 1 (=amb-crawl mytree)))

(deftest test-dft-continuations
         (test-amb [1 2 3 4 5 6 7] (=amb-crawl mytree))
         (test-amb [1 2 3 4 5 6 7] (=iterate-crawl mytree)))

(=defn multi-crawl-1 [_]
       (=bind [node1 (=amb-crawl [1 [2 3]])
               node2 (=amb-crawl [[4 5] 6])]
              (=return [node1 node2])))

(=defn multi-crawl-2 [_]
       (=bind [node1 (=amb-crawl [1 2 3])
               node2 (=amb-crawl [4 5 6])]
              (=return [node1 node2])))

(deftest test-more-dft-continuations
  (test-amb [[1 4] [1 5] [1 6]
             [2 4] [2 5] [2 6]
             [3 4] [3 5] [3 6]]
            (=multi-crawl-1 nil))
  (test-amb [[1 4] [1 5] [1 6]
             [2 4] [2 5] [2 6]
             [3 4] [3 5] [3 6]]
            (=multi-crawl-2 nil)))

(=defn crawl-test [_]
       (=bind [node1 (=ambv 1 2 3)
               node2 (=ambv 4 5 6)]
              (=return [node1 node2])))

(=defn iterate-crawl-test [_]
       (=bind [node1 (=amb-iterate [1 2 3])
               node2 (=amb-iterate [4 5 6])]
              (=return [node1 node2])))

(deftest test-amb
  (test-amb [[1 4] [1 5] [1 6]
             [2 4] [2 5] [2 6]
             [3 4] [3 5] [3 6]]
    (=crawl-test nil))
  (test-amb [[1 4] [1 5] [1 6]
          [2 4] [2 5] [2 6]
          [3 4] [3 5] [3 6]]
    (=iterate-crawl-test nil))
  (test-amb [] (=amb))
  (test-amb [] (=ambv))
  (test-amb [] (=amb-iterate [])))
