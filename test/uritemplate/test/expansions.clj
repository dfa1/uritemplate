(ns uritemplate.test.expansions
  (:use [uritemplate.expansions])
  (:use [clojure.test]))

(deftest char-range-test
  (is (= [] (char-range \z \a)))
  (is (= [\a] (char-range \a \a)))
  (is (= [\a \b] (char-range \a \b))))

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

(deftest truncate-test
  (is (= "" ((truncate-to -1) "")))
  (is (= "" ((truncate-to 0) "")))
  (is (= "" ((truncate-to 1) "")))
  (is (= "val" ((truncate-to 3) "value")))
  (is (= "value" ((truncate-to 5) "value")))
  (is (= "value" ((truncate-to 9999) "value"))))

(deftest simple-expansion-test
  (is (= "val"
         (expand {:type :simple :vars [{:name "var" :maxlen 3}]} {:var "value"}))))

(deftest form-expansion-test
  (is (= "&var=value"
         (expand {:type :formcont :vars [{:name "var" }]} {:var "value"}))))


