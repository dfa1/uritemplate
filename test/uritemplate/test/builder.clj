(ns uritemplate.test.builder
  (:use [uritemplate.builder])
  (:use [clojure.test]))

(deftest lexer-accept-empty-string
    (is (= nil (lexer ""))))

(deftest lexer-accept-literal-string
    (is (= '("www.example.com") (lexer "www.example.com"))))

(deftest lexer-accept-expression-string
    (is (= '("{.dom*}") (lexer "{.dom*}"))))

(deftest lexer-accept-expressions-interleaved-by-literals
    (is (= '("www" "{.dom*}" "/" "{context}") (lexer "www{.dom*}/{context}"))))



(deftest removing-braces
  (is (= "foo" (remove-braces "{foo}"))))



(deftest variable-exploding
  (is (= {:modifier :explode :name "foo"} (parse-variable "foo*"))))

(deftest variable-prefixing
  (is (= {:modifier :prefix :name "foo" :arg "123"} (parse-variable "foo:123"))))

(deftest variable-simple
  (is (= {:modifier :none :name "foo"} (parse-variable "foo"))))

(deftest accept-singleton-variable
  (is (= '({:modifier :none :name "foo"})
         (parse-variable-list "foo"))))

(deftest accept-multiple-variables
  (is (= '({:modifier :none :name "foo"} {:modifier :none :name "bar"})
         (parse-variable-list "foo,bar"))))



(deftest accept-literal
  (is (= {:type :literal :value "http://example.com"} (parse "http://example.com"))))

(deftest accept-simple-expression
  (is (= '({:type :simple :modifier :none :name "foo"}) (parse "{foo}"))))

(deftest accept-reserved-expression
  (is (= '({:type :reserved :modifier :none :name "foo"}) (parse "{+foo}"))))

(deftest accept-fragment-expression
  (is (= '({:type :fragment :modifier :none :name "foo"}) (parse "{#foo}"))))



