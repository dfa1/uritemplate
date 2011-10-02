(ns uritemplate.test.type1
  (:use [uritemplate.type1])
  (:use [clojure.test]))

;; keyword to uri parameter
(deftest accept-keyword
  (is (= "{foo}" (keyword->param :foo))))

(deftest refuse-non-keyword
  (is (thrown? AssertionError (keyword->param ":foo"))))

(deftest refuse-nil
  (is (thrown? AssertionError (keyword->param nil))))

;; uri templates
(deftest refuse-nil-template
  (is (thrown? AssertionError (uri-template nil))))

(deftest refuse-empty-template
  (is (thrown? AssertionError (uri-template ""))))

(deftest accept-constant-template
  (is (= "http://example.com" ((uri-template "http://example.com")))))

(deftest parameter-expansion-is-url-encoded
  (let [template (uri-template "http://example.com/{name}")]
    (is (= "http://example.com/davide+angelocola"
           (template :name "davide angelocola")))))

(deftest one-parameter-template
  (let [template (uri-template "http://example.com/users/{nick}")]
    (is (= "http://example.com/users/dfa"
           (template :nick "dfa")))))

(deftest one-parameter-multiple-time
  (let [template (uri-template "http://{nick}.example.com/{nick}")]
    (is (= "http://dfa.example.com/dfa"
           (template :nick "dfa")))))

(deftest multiple-parameters-template
  (let [template (uri-template "http://example.com/{role}/{nick}")]
    (is (= "http://example.com/user/dfa"
           (template :nick "dfa" :role "user")))))

(deftest numeric-parameters-template
  (let [template (uri-template "http://example.com/{id}")]
    (is (= "http://example.com/42"
           (template :id 42)))))

(deftest can-expand-twice-same-parameter-template
  (let [template (uri-template "http://example.com/{x}/{x}")]
    (is (= "http://example.com/5/5"
           (template :x "5")))))

