(ns uritemplate.test.expansions
  (:use [uritemplate.expansions])
  (:use [clojure.test]))

(deftest render-smoke-tests
  (is (= "/1,2,3" (render "/" "," '(1 2 3))))
  (is (= "/foo" (render "/" "" "foo"))))
