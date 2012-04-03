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
  (is (= "1,2,3" (render "," '(1 2 3) identity false)))
  (is (= "foo" (render "" "foo" identity false))))

(deftest truncate-to-test
  (is (= nil (truncate-to nil 0)))
  (is (= "val" (truncate-to "value" 3)))
  (is (= "value" (truncate-to "value" 5)))
  (is (= "value" (truncate-to "value" 9999))))

(deftest simple-expansion-test
  (is (= "val" (expand {:type :simple :vars [{:name "var" :maxlen 3}]}
                       {:var "value"} ))))

(deftest explode-test
  (let [keys {"semi" ";" "dot" "." "comma" ","}]
    (is (= "dot=.,semi=%3B,comma=%2C" (explode keys "," urlencode)))))
