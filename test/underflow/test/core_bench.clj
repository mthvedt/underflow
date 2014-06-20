(ns underflow.test.core-bench
  (require [clojure.test :as t])
  (use underflow.core underflow.test.core criterium.core))

(defmacro defbench [thename microexpr]
  `(do
     (println "Benchmarking: " ~thename)
     (with-progress-reporting (bench ~microexpr))
     (println "====")))

(defn- recursive-fib [x]
  (case x
    0 1
    1 1
    (+ (recursive-fib (- x 1)) (recursive-fib (- x 2)))))

(defn- fast-fib [i]
  (loop [i (unchecked-subtract (int i) 1) x1 (int 1) x2 (int 1)]
    (if (= i 0)
      x2
      (recur (unchecked-subtract i 1)
             x2
             (unchecked-add x1 x2)))))

(t/deftest fast-fib-check
  (t/is (= (recursive-fib 15) (underflow fib 15)))
  (t/is (= (fast-fib 15) (underflow fib 15))))

(defbench "Recursive fib" (recursive-fib 15))
(defbench "Fast fib" (fast-fib 15))
(defbench "Underflow fib" (underflow fib 15))

(defn crawl-tree [tree]
  (if (coll? tree)
    (mapcat crawl-tree tree)
    (list tree)))

(def test-tree [[[[1 2] 3] [4 5]] [6 [[7 8] [9 10]]]])

(t/deftest test-tree-check
  (t/is (= (vec (crawl-tree test-tree)) (vec (underflow-seq (=dft2 test-tree)))))
  (t/is (= [1 2 3 4 5 6 7 8 9 10] (vec (underflow-seq (=dft2 test-tree))))))

(defbench "Tree crawler" (dorun (crawl-tree test-tree)))
(defbench "Underflow tree crawler" (dorun (underflow-seq (=dft2 test-tree))))
