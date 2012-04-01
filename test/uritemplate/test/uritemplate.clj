(ns uritemplate.test.uritemplate
  (:use [uritemplate.uritemplate])
  (:use [clojure.test]))

(deftest lexer-accept-empty-string
    (is (= nil (lexer ""))))

(deftest lexer-accept-literal-string
    (is (= '("www.example.com") (lexer "www.example.com"))))

(deftest lexer-accept-expression-string
    (is (= '("{.dom*}") (lexer "{.dom*}"))))

(deftest lexer-accept-expressions-interleaved-by-literals
    (is (= '("www" "{.dom*}" "/" "{context}") (lexer "www{.dom*}/{context}"))))


(deftest get-variable-list-test
  (is (= ""         (get-variable-list "{}")))
  (is (= "foo"      (get-variable-list "{foo}")))
  (is (= "foo*"     (get-variable-list "{foo*}")))
  (is (= "foo:10"   (get-variable-list "{foo:10}")))
  (is (= "foo,bar"  (get-variable-list "{foo,bar}"))))


(deftest variable-exploding
  (is (= {:explode true :name "foo"} (parse-variable "foo*"))))

(deftest variable-prefix-maxlen-is-saved
  (is (= {:name "foo" :maxlen 123} (parse-variable "foo:123"))))

(deftest variable-maxlen-must-be-less-than-10000
  (is (thrown? AssertionError (parse-variable "foo:10000"))))

(deftest variable-maxlen-must-be-positive
  (is (thrown? AssertionError (parse-variable "foo:0"))))

(deftest variable-simple
  (is (= {:name "foo"} (parse-variable "foo"))))

(deftest accept-singleton-variable
  (is (= '({:name "foo"})
         (parse-variable-list "foo"))))

(deftest accept-multiple-variables
  (is (= '({:name "foo"} {:name "bar"})
         (parse-variable-list "foo,bar"))))


(deftest accept-literal
  (is (= {:type :literal :value "http://example.com"} (parse "http://example.com"))))

(deftest accept-simple-expression
  (is (= {:type :simple :vars [{:name "foo"}]}
         (parse "{foo}"))))

(deftest accept-simple-expression-with-two-variables
  (is (= {:type :simple :vars [{:name "foo"} {:name "bar"}]}
         (parse "{foo,bar}"))))

(deftest accept-expression-without-variables
  (is (= {:type :simple :vars []}
         (parse "{}"))))


(deftest readme-example
  (let [bitbucket (uritemplate "http://bitbucket.org/{user}/{project}")]
    (is (= "http://bitbucket.org/dfa/uritemplate"
           (bitbucket {:user "dfa" :project "uritemplate"})))))
