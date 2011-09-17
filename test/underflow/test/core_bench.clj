(ns underflow.test.core-bench
  (require [clojure.test :as t])
  (use underflow.core underflow.test.core criterium.core))

(defmacro defbench [thename microexpr]
  `(do
     (println "Benchmarking: " ~thename)
     (with-progress-reporting (bench ~microexpr))
     (println "====")))

(defn- recursive-consecutive-sum [x x2]
  (if (= x 0)
    x2
    (recursive-consecutive-sum (dec x) (+ x x2))))

(defn- fast-consecutive-sum [xobj]
  (let [minusone (int -1)
        zero (int 0)
        x (int xobj)]
    (loop [i x
           thesum zero]
      (if (= i zero)
        thesum
        (recur (unchecked-add i minusone) (unchecked-add i thesum))))))

(defbench "Recursive consecutive sum" (recursive-consecutive-sum 1000 0))
(defbench "Fast consecutive sum" (fast-consecutive-sum 1000))
(defbench "Underflow consecutive sum" (reset= consecutive-sum 1000 0))

(defn crawl-tree [tree]
  (if (coll? tree)
    (mapcat crawl-tree tree)
    (list tree)))

(def test-tree [[[[1 2] 3] [4 5]] [6 [[7 8] [9 10]]]])

(t/deftest test-tree-check
  (t/is (= (vec (crawl-tree test-tree)) (vec (>=dft2 test-tree))))
  (t/is (= [1 2 3 4 5 6 7 8 9 10] (vec (>=dft2 test-tree)))))

(defbench "Tree crawler" (dorun (crawl-tree test-tree)))
(defbench "Underflow tree crawler" (dorun (>=dft2 test-tree)))
