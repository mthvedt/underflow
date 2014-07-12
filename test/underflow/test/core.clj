(ns underflow.test.core
  (use clojure.test underflow.core)
  (:refer-clojure :exclude [pop!]))

(=defn test1 [x] (=return x))

(deftest test-defs
         (is (= (=underflow (=test1 1)) 1)))

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
  (is (=underflow (=funny-even? 10)))
  (is (not (=underflow (=funny-even? 11))))
  (is (=underflow (=funny-odd? 11)))
  (is (not (=underflow (=funny-odd? 10)))))

(=defn fib [x]
       (case x
         0 (=return 1)
         1 (=return 1)
         (=bind [a (=fib (- x 1))
                 b (=fib (- x 2))]
                (=return (+ a b)))))

; TODO test failures. premature return shouldnt be allowed
; (the below tests without return should fail)
(deftest test-let-and-bind
  (is (= 6 (=underflow (=let [z (+ 1 2)]
                             (=return (+ z 3))))))
  (is (= 9 (=underflow (=let [z (+ 1 2)
                              z2 (+ z 3)]
                             (=return (+ z z2))))))
  (is (= 9 (=underflow (=let [z (+ 1 2)]
                             (=let [z2 (+ z 3)]
                                   (=return (+ z z2)))))))
  (is (= (=underflow (=fib 5)) 8))
  (is (= (=underflow (=fib 10)) 89)))

(def mytree [[[1 2] 3] [[4 5] [6 7]]])
; TODO names
(=defn dft2 [tree]
       (if (coll? tree)
         (if (seq tree)
           (=amb (=dft2 (first tree)) (=dft2 (rest tree)))
           (=retry))
         (=return tree)))

; TODO better name than apply amb
; TODO move to tests
(=defn dft3 [tree]
  (if (coll? tree)
    ; apply-amb must be in tail position
    (=bind [x (=amb-iterate tree)]
           (=dft3 x))
    (=return tree)))

(deftest test-dft2
  (is (= 1 (=underflow (=dft2 mytree)))))

(deftest test-dft-continuations
         (is (= [1 2 3 4 5 6 7] (vec (underflow-seq (=dft2 mytree)))))
         (is (= [1 2 3 4 5 6 7] (vec (underflow-seq (=dft3 mytree))))))

; TODO a macro
(defn dftx [tree1 tree2]
  (vec
    (underflow-seq
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
    (underflow-seq
      (=bind [node1 (=ambv 1 2 3)
              node2 (=ambv 4 5 6)]
            (=return [node1 node2])))))

(defn dft-iterate []
  (vec
    (underflow-seq
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
  (is (= (underflow-seq (=amb)) nil))
  (is (= (underflow-seq (=ambv)) nil))
  (is (= (underflow-seq (=amb-iterate [])) nil)))
