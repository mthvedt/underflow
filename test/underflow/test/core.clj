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
       (case (int x)
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
