(ns underflow.test.core
  (use clojure.test underflow.core)
  (:refer-clojure :exclude [pop!]))

(=defn test1 [x] x)

(deftest test-defs
         (is (= (underflow test1 1) 1)))

(=declare funny-odd?)

(=defn funny-even? [x]
       (if (= x 0)
         true
         (=tailcall (=funny-odd? (dec x)))))

; TODO maybe tailcall can be simplified?
(=defn funny-odd? [x]
       (if (= x 0)
         false
         (=tailcall (=funny-even? (dec x)))))

(deftest test-tailrecur
  (is (underflow funny-even? 1000000))
  (is (not (underflow funny-even? 1000001)))
  (is (underflow funny-odd? 10000001))
  (is (not (underflow funny-odd? 100000))))

(=defn fib [x]
       (case x
         0 1
         1 1
         (=let [a (=fib (- x 1))
                b (=fib (- x 2))]
               (+ a b))))

(deftest test-let
  (is (= 6 (=underflow (=let [z (+ 1 2)] (+ z 3)))))
  (is (= 9 (=underflow (=let [z (+ 1 2) z2 (+ z 3)]
                             (+ z z2)))))
  (is (= 9 (=underflow (=let [z (+ 1 2)]
                             (=let [z2 (+ z 3)]
                                   (+ z z2))))))
  (is (= (underflow fib 5) 8))
  (is (= (underflow fib 10) 89)))

(def mytree [[[1 2] 3] [[4 5] [6 7]]])
(=defn dft2 [tree]
       (if (coll? tree)
         (if (seq tree)
           (do
             (=save! (=dft2 (rest tree)))
             (=dft2 (first tree)))
           (=retry))
         tree))

(deftest test-dft2
  (is (= 1 (underflow dft2 mytree))))

(deftest test-dft-continuations
         (is (= [1 2 3 4 5 6 7] (vec (underflow-seq (=dft2 mytree))))))

; TODO a macro
(defn dftx [tree1 tree2]
  (vec
    (underflow-seq
      (=let [node1 (=dft2 tree1)
             node2 (=dft2 tree2)]
            [node1 node2]))))

(deftest test-more-dft-continuations
  (is (= (dftx [[1 2] 3] [4 [5 6]])
         [[1 4] [1 5] [1 6]
          [2 4] [2 5] [2 6]
          [3 4] [3 5] [3 6]])))

(defn dftamb []
  (vec
    (underflow-seq
      (=let [node1 (=amb 1 2 3)
             node2 (=amb 4 5 6)]
            [node1 node2]))))

; TODO rename
(deftest test-amb
  (is (= (dftamb)
         [[1 4] [1 5] [1 6]
          [2 4] [2 5] [2 6]
          [3 4] [3 5] [3 6]])))
