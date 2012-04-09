(ns uritemplate.test.uritemplate
  (:use [uritemplate.uritemplate])
  (:use [clojure.test]))

(deftest tokenize-test
  (is (= nil (tokenize "")))
  (is (= ["www.example.com"] (tokenize "www.example.com")))
  (is (= ["{.dom*}"] (tokenize "{.dom*}")))
  (is (= ["www" "{.dom*}" "/" "{context}"] (tokenize "www{.dom*}/{context}")))
  (is (= ["up" "{+path}" "{var}" "/here"] (tokenize "up{+path}{var}/here")))
  (is (= ["{/path}"] (tokenize "{/path}"))))

(deftest variables-test
  (is (= {:explode true :name "foo"} (parse-variable "foo*")))
  (is (= {:name "foo" :maxlen 123} (parse-variable "foo:123")))
  (is (thrown? AssertionError (parse-variable "foo:10000")))
  (is (thrown? AssertionError (parse-variable "foo:0")))
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

(deftest expression-test
  (is (= {:type :literal :value "{}"} (parse-token "{}"))))
