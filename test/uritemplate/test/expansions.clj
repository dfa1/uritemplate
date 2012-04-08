(ns uritemplate.test.expansions
  (:use [uritemplate.expansions])
  (:use [clojure.test]))

(deftest pct-encode-test
  (is (= "%20" (pct-encode \space)))
  (is (= "%2F" (pct-encode \/))))

(deftest urlencode-test
  (is (= "hello" (urlencode "hello")))
  (is (= "hello%20world%21" (urlencode "hello world!"))))

(deftest urlencode-reserved-test
  (is (= "hello" (urlencode-reserved "hello")))
  (is (= "hello%20world!" (urlencode-reserved "hello world!"))))

(deftest render-test
  (is (= "1,2,3" (render "," '(1 2 3) identity false 4)))
  (is (= "foo" (render "" "foo" identity false 3))))

(deftest truncate-to-test
  (is (= "" (truncate "" 0)))
  (is (= "" (truncate "" 1)))
  (is (= "val" (truncate "value" 3)))
  (is (= "value" (truncate "value" 5)))
  (is (= "value" (truncate "value" 9999))))

(deftest simple-expansion-test
  (is (= "val" (expand {:type :simple :vars [{:name "var" :maxlen 3}]}
                       {:var "value"} ))))

