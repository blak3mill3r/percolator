(ns percolator.test (:use clojure.test))

(use 'clojure.test)

(defmacro expr-to-string [expr] (.toString (eval (percolator.core/interpret-expression expr))))

(deftest binary-ops
  (is (= (expr-to-string ('+   3 2 )) "3 + 2"   )) 
  (is (= (expr-to-string ('-   3 2 )) "3 - 2"   )) 
  (is (= (expr-to-string ('*   3 2 )) "3 * 2"   )) 
  (is (= (expr-to-string ('/   3 2 )) "3 / 2"   )) 
  (is (= (expr-to-string ('==  3 2 )) "3 == 2"  )) 
  (is (= (expr-to-string ('!=  3 2 )) "3 != 2"  )) 
  (is (= (expr-to-string ('<=  3 2 )) "3 <= 2"  )) 
  (is (= (expr-to-string ('>=  3 2 )) "3 >= 2"  )) 
  (is (= (expr-to-string ('<<  3 2 )) "3 << 2"  )) 
  (is (= (expr-to-string ('>>  3 2 )) "3 >> 2"  )) 
  (is (= (expr-to-string ('>>> 3 2 )) "3 >>> 2" )) 
  (is (= (expr-to-string ('%   3 2 )) "3 % 2"   )) 
  ;(is (= (expr-to-string ('* ('+ 5 2) 2)) "(5 + 2) * 2")) ; this one fails because japaparser fails at order of operations
         )

(deftest func-calls
         (is (= (expr-to-string ('. twongle spiff)) "twongle.spiff()"))
         (is (= (expr-to-string ('. twongle spiff 3)) "twongle.spiff(3)"))
         (is (= (expr-to-string ('. twongle spiff 3 4)) "twongle.spiff(3, 4)"))
         (is (= (expr-to-string ('. twongle spiff 3 ('+ 1 2))) "twongle.spiff(3, 1 + 2)"))
         )

(deftest test-something
  (testing "Simple expressions"
    (testing "binary operator expressions" (binary-ops)))
    (testing "function call expressions" (func-calls)))

(run-tests)

