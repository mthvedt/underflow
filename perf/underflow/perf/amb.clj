(ns underflow.perf.amb
  (require [clojure.test :as t])
  (use underflow.core underflow.amb underflow.test.amb criterium.core))

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
  (t/is (= (recursive-fib 15) (underflow (fast-harness) (=fib 15))))
  (t/is (= (fast-fib 15) (underflow (fast-harness) (=fib 15)))))

(defbench "Recursive fib" (recursive-fib 15))
(defbench "Fast fib" (fast-fib 15))
(defbench "Underflow fib" (underflow (fast-harness) (=fib 15)))

(defn crawl-tree [tree]
  (if (coll? tree)
    (mapcat crawl-tree tree)
    (list tree)))

(def test-tree [[[[1 2] 3] [4 5]] [6 [[7 8] [9 10]]]])

(t/deftest test-tree-check
  (t/is (= (vec (crawl-tree test-tree))
           (vec (underflow-seq (fast-harness) (=amb-crawl test-tree)))))
  (t/is (= (vec (crawl-tree test-tree))
           (vec (underflow-seq (fast-harness) (=iterate-crawl test-tree)))))
  (t/is (= (vec (crawl-tree test-tree))
           (vec (underflow-seq (fast-harness) (=iterate-crawl-2 test-tree)))))
  (t/is (= (vec (crawl-tree test-tree))
           (vec (underflow-seq (fast-harness) (=iterate-crawl-3 test-tree)))))
  (t/is (= [1 2 3 4 5 6 7 8 9 10] (vec (underflow-seq (fast-harness)
                                                      (=amb-crawl test-tree))))))

(defn dorun-iterator [^java.util.Iterator i]
  (while (.hasNext i)
    (.next i)))

(defbench "Tree crawler" (doall (crawl-tree test-tree)))
(defbench "Underflow tree crawler" (doall (underflow-seq (fast-harness)
                                                         (=amb-crawl test-tree))))
(defbench "Underflow tree crawler iterator"
  (dorun-iterator (underflow-iterator (fast-harness) (=amb-crawl test-tree))))
(defbench "Fast underflow tree crawler iterator (warm-up)"
  (dorun-iterator (underflow-iterator (fast-harness) (=iterate-crawl test-tree))))
(defbench "Fast underflow tree crawler iterator, with =>return"
  (dorun-iterator (underflow-iterator (fast-harness) (=iterate-crawl-2 test-tree))))
(defbench "Fast underflow tree crawler iterator, with inline macros"
  (dorun-iterator (underflow-iterator (fast-harness) (=iterate-crawl-3 test-tree))))
(defbench "Fast underflow tree crawler redux"
  (dorun-iterator (underflow-iterator (fast-harness) (=iterate-crawl test-tree))))
