(ns uritemplate.test.encoding
  (:use [uritemplate.encoding])
  (:use [clojure.test]))

(deftest refuse-url-encoding-nil
  (is (thrown? AssertionError (url-encode nil))))

(deftest can-encode-a-string
  (is (= "http%3A%2F%2Fexample.com" (url-encode "http://example.com"))))

(deftest can-encode-a-number
  (is (= "1" (url-encode 1))))

(deftest can-encode-an-empty-string
  (is (= "" (url-encode ""))))
