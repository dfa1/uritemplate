(ns uritemplate.test.expansions
  (:use [uritemplate.expansions])
  (:use [clojure.test]))

(deftest join-test
  (is (= "" (join "," [])))
  (is (= "a" (join "," ["a"])))
  (is (= "a,b" (join "," ["a","b"]))))

(deftest join-with-prefix-test
  (is (= "" (join-with-prefix "/" "," [])))
  (is (= "" (join-with-prefix "/" "," [nil])))
  (is (= "/" (join-with-prefix "/" "," [""])))
  (is (= "/a" (join-with-prefix "/" "," [nil, "a"])))
  (is (= "/a" (join-with-prefix "/" "," ["a", nil])))
  (is (= "/a" (join-with-prefix "/" "," ["a"])))
  (is (= "/a,b" (join-with-prefix "/" "," ["a","b"]))))

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

(deftest name-render-test
  (is (= "list=foo;list=bar"
         (name-render ";" "" "list" ["foo", "bar"] true 200)))
  (is (= "list=foo;list=bar"
         (name-render ";" "" "list" ["foo", "bar"] true 200))))

(deftest truncate-to-test
  (is (= "" (truncate "" 0)))
  (is (= "" (truncate "" 1)))
  (is (= "val" (truncate "value" 3)))
  (is (= "value" (truncate "value" 5)))
  (is (= "value" (truncate "value" 9999))))

(deftest simple-expansion-test
  (is (= "val"
         (expand {:type :simple :vars [{:name "var" :maxlen 3}]} {:var "value"}))))

(deftest form-expansion-test
  (is (= "&var=value"
         (expand {:type :formcont :vars [{:name "var" }]} {:var "value"}))))


