(ns underflow.test.core
  (use clojure.test underflow.core))

(defmacro test-underflow [expected & body]
  `(let [expected# ~expected
         testf# (fn [state#] (underflow state# ~@body))]
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

(def mytree [[[1 2] 3] [[4 5] [6 7]]])
; TODO names
(=defn dft2 [tree]
       (if (coll? tree)
         (if (seq tree)
           (=amb (=dft2 (first tree)) (=dft2 (rest tree)))
           (=retry))
         (=return tree)))

(=defn dft3 [tree]
  (if (coll? tree)
    (=bind [x (=amb-iterate tree)]
           (=dft3 x))
    (=return tree)))

(deftest test-dft2
  (test-underflow 1 (=dft2 mytree)))

(deftest test-dft-continuations
         (is (= [1 2 3 4 5 6 7] (vec (underflow-seq (fast-harness) (=dft2 mytree)))))
         (is (= [1 2 3 4 5 6 7] (vec (underflow-seq (fast-harness) (=dft3 mytree))))))

(defn dftx [tree1 tree2]
  (vec
    (underflow-seq (fast-harness)
      (=bind [node1 (=dft2 tree1)
              node2 (=dft2 tree2)]
            (=return [node1 node2])))))

(deftest test-more-dft-continuations
  (is (= (dftx [[1 2] 3] [4 [5 6]])
         [[1 4] [1 5] [1 6]
          [2 4] [2 5] [2 6]
          [3 4] [3 5] [3 6]])))

(defn dftamb []
  (vec
    (underflow-seq (fast-harness)
      (=bind [node1 (=ambv 1 2 3)
              node2 (=ambv 4 5 6)]
            (=return [node1 node2])))))

; TODO test safe-harness
(defn dft-iterate []
  (vec
    (underflow-seq (fast-harness)
      (=bind [node1 (=amb-iterate [1 2 3])
              node2 (=amb-iterate [4 5 6])]
            (=return [node1 node2])))))

; TODO rename
(deftest test-amb
  (is (= (dftamb)
         [[1 4] [1 5] [1 6]
          [2 4] [2 5] [2 6]
          [3 4] [3 5] [3 6]]))
  (is (= (dft-iterate)
         [[1 4] [1 5] [1 6]
          [2 4] [2 5] [2 6]
          [3 4] [3 5] [3 6]]))
  (is (= (underflow-seq (fast-harness) (=amb)) nil))
  (is (= (underflow-seq (fast-harness) (=ambv)) nil))
  (is (= (underflow-seq (fast-harness) (=amb-iterate [])) nil)))
