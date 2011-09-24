(ns uritemplate.test
  (:use [uritemplate])
  (:use [clojure.test]))

;; abusing testing macro...

(testing "uri parameter to keyword conversion" 

  (deftest parameter
    (is (= :foo (param->keyword "{foo}"))))

  (deftest parameter-with-semicolon
    (is (= :foo (param->keyword "{:foo}"))))
  
  (deftest refuse-empty-parameter
    (is (thrown? AssertionError (param->keyword ""))))

  (deftest refuse-nil-parameter
    (is (thrown? AssertionError (param->keyword nil))))

  (deftest refuse-url-parameters-with-spaces
    (is (thrown? AssertionError (param->keyword "{foo bar}")))))


(testing "keyword to uri parameter"

  (deftest accept-keyword
    (is (= "{foo}" (keyword->param :foo))))

  (deftest refuse-non-keyword
    (is (thrown? AssertionError (keyword->param ":foo"))))
  
  (deftest refuse-nil
    (is (thrown? AssertionError (keyword->param nil)))))


(testing "extract parameters"

  (deftest extract-parameter
    (is (= (list :foo) (extract-params "http://host/{foo}"))))

  (deftest multiple-parameters-in-order
    (is (= (list :foo :bar) (extract-params "http://host/{foo}/{bar}")))))


(testing "level 1 uri templates" 

  (deftest refuse-nil-template
    (is (thrown? AssertionError (url-template nil))))

  (deftest refuse-empty-template
    (is (thrown? AssertionError (url-template ""))))

  (deftest accept-constant-template
    (is (= "http://example.com" ((url-template "http://example.com")))))

  (deftest parameter-expansion-is-url-encoded
    (let [template (url-template "http://example.com/{name}")]
      (is (= "http://example.com/davide+angelocola"
             (template :name "davide angelocola")))))

  (deftest one-parameter-template
    (let [template (url-template "http://example.com/users/{nick}")]
      (is (= "http://example.com/users/dfa"
             (template :nick "dfa")))))

  (deftest one-parameter-multiple-time
    (let [template (url-template "http://{nick}.example.com/{nick}")]
      (is (= "http://dfa.example.com/dfa"
             (template :nick "dfa")))))

  (deftest multiple-parameters-template
    (let [template (url-template "http://example.com/{role}/{nick}")]
      (is (= "http://example.com/user/dfa"
             (template :nick "dfa" :role "user")))))

  (deftest numeric-parameters-template
    (let [template (url-template "http://example.com/{id}")]
      (is (= "http://example.com/42"
             (template :id 42))))))
