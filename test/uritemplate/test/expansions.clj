(ns uritemplate.test.expansions
  (:use [uritemplate.expansions])
  (:use [clojure.test]))

(deftest output-smoke-tests
  (is (= "/1,2,3" (output "/" "," '(1 2 3))))
  (is (= "/foo" (output "/" "" "foo"))))
