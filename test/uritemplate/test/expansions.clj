(ns uritemplate.test.expansions
  (:use [uritemplate.expansions])
  (:use [clojure.test]))

(deftest render-smoke-tests
  (is (= "/1,2,3" (render "/" "," '(1 2 3))))
  (is (= "/foo" (render "/" "" "foo"))))

(deftest truncate-to-smoke-test
  (is (= "val" (truncate-to "value" 3)))
  (is (= "value" (truncate-to "value" 5)))
  (is (= "value" (truncate-to "value" 9999))))

(deftest simple-string-expansion 
  (is (= "val" (expand {:type :simple :vars [{:name "var" :maxlen 3}]}
                       {:var "value"}))))
