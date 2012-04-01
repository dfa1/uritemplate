(ns uritemplate.test.expansions
  (:use [uritemplate.expansions])
  (:use [clojure.test]))

(deftest urlencode-test
  (is (= "hello" (urlencode "hello")))
  (is (= "hello%20world%21" (urlencode "hello world!"))))

(deftest urlencode-reserved-test
  (is (= "hello" (urlencode-reserved "hello")))
  (is (= "hello%20world!" (urlencode-reserved "hello world!"))))

(deftest render-test
  (is (= "1,2,3" (render "," '(1 2 3) identity)))
  (is (= "foo" (render "" "foo" identity))))

(deftest truncate-to-test
  (is (= "val" (truncate-to "value" 3)))
  (is (= "value" (truncate-to "value" 5)))
  (is (= "value" (truncate-to "value" 9999))))

(deftest simple-expansion-test
  (is (= "val" (expand {:type :simple :vars [{:name "var" :maxlen 3}]}
                       {:var "value"}))))
