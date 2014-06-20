(ns underflow.test.core
  (:use clojure.test underflow.core))

(=defn test1 [x] x)

(deftest test-defs
         (is (= (underflow test1 1) 1)))

(declare funny-odd?)

(=defn funny-even? [x]
       (if (= x 0)
         true
         (=tailrecur funny-odd? (dec x))))

(=defn funny-odd? [x]
       (if (= x 0)
         false
         (=tailrecur funny-even? (dec x))))

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
  (cond (nil? tree) (=retry)
        (and (coll? tree) (seq tree)) (=save (=dft2 (first tree)) (=dft2 (rest tree)))
        (coll? tree) (=retry)
        true tree))

#_(let [x (doall (underflow-seq dft2 mytree))]
  (prn x))

(deftest test-dft-continuations
         (is (= [1 2 3 4 5 6 7] (vec (underflow-seq dft2 mytree)))))

#_(=defn dftx [tree1 tree2]
  (=let [node1 (=dft-node tree1)]
        (if (= ::done node1)
          ::done
          (=let [node2 (=dft-node tree2)]
                (do (set! *rval* (conj *rval* [node1 node2]))
                  (restart))))))

#_(defn >=dftx [tree1 tree2]
  (binding [*tree-stack* []
            *rval* []]
    (underflow dftx tree1 tree2)
    *rval*))

#_(deftest test-more-dft-continuations
         (is (= (>=dftx [[1 2] 3] [4 [5 6]])
                [[1 4] [1 5] [1 6]
                 [2 4] [2 5] [2 6]
                 [3 4] [3 5] [3 6]])))
