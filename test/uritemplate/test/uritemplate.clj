(ns uritemplate.test.uritemplate
  (:use [uritemplate.uritemplate])
  (:use [clojure.test]))

(deftest split-by-test
  (let [comma? (ch \,)]
  (is (= [] (split-by comma? (seq ""))))
  (is (= [] (split-by comma? (seq ","))))
  (is (= [[\a]] (split-by comma? (seq "a"))))
  (is (= [[\a]] (split-by comma? (seq "a,"))))
  (is (= [[\a]] (split-by comma? (seq ",a"))))
  (is (= [[\a] [\b]] (split-by comma? (seq "a,b"))))
  (is (= [[\a] [\b]] (split-by comma? (seq "a,,b"))))))

(deftest tokenize-test
  (is (= nil (tokenize "")))
  (is (= ["www.example.com"] (tokenize "www.example.com")))
  (is (= ["{.dom*}"] (tokenize "{.dom*}")))
  (is (= ["www" "{.dom*}" "/" "{context}"] (tokenize "www{.dom*}/{context}")))
  (is (= ["up" "{+path}" "{var}" "/here"] (tokenize "up{+path}{var}/here")))
  (is (= ["{/path}"] (tokenize "{/path}"))))

(deftest parse-token-test
  (is (= {:type :literal :value "foo"} (parse-token "foo")))
  (is (= {:type :simple :vars []} (parse-token "{}")))
  (is (= {:type :simple :vars [{:name "foo"}]} (parse-token "{foo}"))))

(deftest parse-varspec-test
  (is (= {:explode true :name "foo"} (parse-varspec (seq "foo*"))))
  (is (= {:name "foo" :maxlen 123} (parse-varspec (seq "foo:123"))))
  (is (thrown? AssertionError (parse-varspec "foo:10000")))
  (is (thrown? AssertionError (parse-varspec "foo:0")))
  (is (= [{:name "foo"}] (parse-variable-list "foo")))
  (is (= [{:name "foo"} {:name "bar"}] (parse-variable-list "foo,,bar")))
  (is (= [{:name "foo"} {:name "bar"}] (parse-variable-list "foo,bar"))))

(deftest literal-test
  (is (= {:type :literal :value "http://example.com"}
         (parse-token "http://example.com"))))

(deftest simple-expression-test
  (is (= {:type :simple :vars [{:name "foo"}]} (parse-token "{foo}")))
  (is (= {:type :simple :vars [{:name "foo"} {:name "bar"}]} (parse-token "{foo,bar}"))))

(deftest reserved-expression-test
  (is (= {:type :reserved :vars [{:name "foo"} {:name "bar"}]} (parse-token "{+foo,bar}"))))

