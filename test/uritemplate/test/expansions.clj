(ns uritemplate.test.expansions
  (:use [uritemplate.expansions])
  (:use [clojure.test]))

(deftest urlencode-test
  (is (= "hello" (urlencode "hello")))
  (is (= "hello%20world%21" (urlencode "hello world!"))))

(deftest render-test
  (is (= "/1,2,3" (render "/" "," '(1 2 3))))
  (is (= "/foo" (render "/" "" "foo"))))

(deftest truncate-to-test
  (is (= "val" (truncate-to "value" 3)))
  (is (= "value" (truncate-to "value" 5)))
  (is (= "value" (truncate-to "value" 9999))))

(deftest simple-expansion-test
  (is (= "val" (expand {:type :simple :vars [{:name "var" :maxlen 3}]}
                       {:var "value"}))))
