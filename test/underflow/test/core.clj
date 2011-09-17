(ns underflow.test.core
  (:use clojure.test underflow.core))

(=defn test1 [x] (=return x))

(deftest test-defs
         (is (= (reset= test1 1) 1))
         (is (= (with-local-vars [*cont* identity] (=test1 1) 1))))

(=defn consecutive-sum [x x2]
       (if (= x 0)
         (=return x2) 
         (=tailcall consecutive-sum (dec x) (+ x x2))))

(declare funny-odd?)

(=defn funny-even? [x]
       (if (= x 0)
         (=return true)
         (=tailcall funny-odd? (dec x))))

(=defn funny-odd? [x]
       (if (= x 0)
         (=return false)
         (=tailcall funny-even? (dec x))))

(deftest test-tailcall
         (is (= 55 (reset= consecutive-sum 10 0)))
         (is (= 50005000 (reset= consecutive-sum 10000 0)))
         (is (reset= funny-even? 1000000))
         (is (not (reset= funny-even? 1000001)))
         (is (reset= funny-odd? 10000001))
         (is (not (reset= funny-odd? 100000))))

(deftest test-let
  (is (= 6 (reset= (=fn [] (=let [z (=return (+ 1 2))] (=return (+ z 3)))))))
  (is (= 9 (reset= (=fn [] (=let [z (=return (+ 1 2)) z2 (=return (+ z 3))]
                                 (=return (+ z z2)))))))
  (is (= 9 (reset= (=fn [] (=let [z (=return (+ 1 2))]
                                 (=let [z2 (=return (+ z 3))]
                                       (=return (+ z z2)))))))))

(def mytree [[[1 2] 3] [[4 5] [6 7]]])

(def ^:dynamic *tree-stack*)
(def ^:dynamic *rval*)

(defn restart []
  (if (empty? *tree-stack*)
    ::done
    (let [cont (first *tree-stack*)]
      (set! *tree-stack* (rest *tree-stack*))
      (cont))))

(=defn dft-node [tree]
  (cond (nil? tree) (restart)
        (coll? tree) (=shift cc
                             (set! *tree-stack*
                                   (cons (fn []
                                           (=let [cval (=dft-node (second tree))]
                                                 (cc cval)))
                                         *tree-stack*))
                             (=dft-node (first tree)))
        true (=return tree)))

(=defn dft2 [tree]
  (=let [node (=dft-node tree)]
        (if (= ::done node)
          (=return *rval*)
          (do (set! *rval* (conj *rval* node))
            (restart)))))

(defn >=dft2 [tree]
  (binding [*tree-stack* []
            *rval* []]
    (reset= dft2 tree)
    *rval*))

(deftest test-dft-continuations
         (is (= [1 2 3 4 5 6 7] (>=dft2 mytree))))

(=defn dftx [tree1 tree2]
  (=let [node1 (=dft-node tree1)]
        (if (= ::done node1)
          ::done
          (=let [node2 (=dft-node tree2)]
                (do (set! *rval* (conj *rval* [node1 node2]))
                  (restart))))))

(defn >=dftx [tree1 tree2]
  (binding [*tree-stack* []
            *rval* []]
    (reset= dftx tree1 tree2)
    *rval*))

(deftest test-more-dft-continuations
         (is (= (>=dftx [[1 2] 3] [4 [5 6]])
                [[1 4] [1 5] [1 6]
                 [2 4] [2 5] [2 6]
                 [3 4] [3 5] [3 6]])))
