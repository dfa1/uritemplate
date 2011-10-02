(ns uritemplate.test.type1
  (:use [uritemplate.type1])
  (:use [clojure.test]))

;; merge-defaults
(deftest empty-merge-defaults
  (is (= {} (merge-defaults {} []))))

(deftest merge-defaults-with-provided-key
  (is (= {:key ""} (merge-defaults {} [:key]))))

(deftest merge-defaults-with-provided-key-and-value
  (is (= {:key :value} (merge-defaults {} :value [:key]))))

(deftest merge-defaults-with-base-map
  (is (= {:foo "" :bar ""} (merge-defaults {:foo ""} [:bar]))))

(deftest merge-defaults-dont-override
  (is (= {:foo "value"} (merge-defaults {:foo "value"} [:foo]))))

;; uri parameter to keyword conversion
(deftest parameter
  (is (= :foo (param->keyword "{foo}"))))

(deftest parameter-with-semicolon
  (is (= :foo (param->keyword "{:foo}"))))

(deftest refuse-empty-parameter
  (is (thrown? AssertionError (param->keyword ""))))

(deftest refuse-nil-parameter
  (is (thrown? AssertionError (param->keyword nil))))

(deftest refuse-url-parameters-with-spaces
  (is (thrown? AssertionError (param->keyword "{foo bar}"))))

;; keyword to uri parameter
(deftest accept-keyword
  (is (= "{foo}" (keyword->param :foo))))

(deftest refuse-non-keyword
  (is (thrown? AssertionError (keyword->param ":foo"))))

(deftest refuse-nil
  (is (thrown? AssertionError (keyword->param nil))))

;; extract parameters
(deftest extract-parameter
  (is (= (list :foo) (extract-params "http://host/{foo}"))))

(deftest multiple-parameters-in-order
  (is (= (list :foo :bar) (extract-params "http://host/{foo}/{bar}"))))

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

(deftest accept-empty-parameter
  (let [template (uri-template "O{empty}X")]
    (is (= "OX" (template :empty "")))))

(deftest accept-undef-parameter
  (let [template (uri-template "O{undef}X")]
    (is (= "OX" (template)))))

