(ns uritemplate.test.uritemplate
  (:use [uritemplate.uritemplate])
  (:use [clojure.test]))

(deftest lexer-test
  (is (= nil (lexer "")))
  (is (= ["www.example.com"] (lexer "www.example.com")))
  (is (= ["{.dom*}"] (lexer "{.dom*}")))
  (is (= ["www" "{.dom*}" "/" "{context}"] (lexer "www{.dom*}/{context}")))
  (is (= ["up" "{+path}" "{var}" "/here"] (lexer "up{+path}{var}/here")))
  (is (= ["{/path}"] (lexer "{/path}"))))

(deftest variables-test
  (is (= {:explode true :name "foo"} (parse-variable "foo*")))
  (is (= {:name "foo" :maxlen 123} (parse-variable "foo:123")))
  (is (thrown? AssertionError (parse-variable "foo:10000")))
  (is (thrown? AssertionError (parse-variable "foo:0")))
  (is (= [{:name "foo"}] (parse-variable-list "foo")))
  (is (= [{:name "foo"} {:name "bar"}] (parse-variable-list "foo,bar"))))

(deftest literal-test
  (is (= {:type :literal :value "http://example.com"}
         (parse "http://example.com"))))

(deftest simple-expression-test
  (is (= {:type :simple :vars [{:name "foo"}]} (parse "{foo}")))
  (is (= {:type :simple :vars [{:name "foo"} {:name "bar"}]} (parse "{foo,bar}"))))

(deftest reserved-expression-test
  (is (= {:type :reserved :vars [{:name "foo"} {:name "bar"}]} (parse "{+foo,bar}"))))

(deftest expression-test
  (is (= {:type :literal :value "{}"} (parse "{}"))))
