(ns uritemplate.test.type4
  (:use [uritemplate.type4])
  (:use [clojure.test]))

(deftest simple-expression-type
  (is (= \v (expression-type "{var}"))))

(deftest reserved-expression-type
  (is (= \+ (expression-type "{+var}"))))

(deftest refuse-empty-expression-type
  (is (thrown? AssertionError (expression-type ""))))

(deftest refuse-expression-type-without-braces
  (is (thrown? AssertionError (expression-type "foo"))))

(deftest refuse-expression-type-with-empty-content
  (is (thrown? AssertionError (expression-type "{}"))))

;; section 3.2
;; expression expansion
(def dom        "example.com")
(def dub        "me/too")
(def foo        "That’s right!")
(def hello      "Hello World!")
(def half       "50%")
(def var        "value")
(def who        "fred")
(def base       "http://example.com/home/")
(def path       "/foo/bar")
(def list       [ "red", "green", "blue" ])
(def keys       {"semi" ";" "dot" "." "comma" ","})
(def v          "6")
(def x          "1024")
(def y          "768")
(def empty      "")
(def empty_keys [])
(def undef      nil)

;; section 3.2.2
;; simple expansion
(deftest expand-string
  (is (= "value" (expand-param var))))

(deftest expand-string-with-slice
  (is (= "val" (expand-param (format "%s:3" var)))))

(deftest expand-string-with-slice-greater-than-value
  (is (= "value" (expand-param (format "%s:30" var)))))

(deftest expand-half
  (is (= "50%25" (expand-param half))))

(deftest expand-list
  (is (= "red,green,blue" (expand-param list))))

(deftest expand-list*
  (is (= "red,green,blue" (expand-param list))))

