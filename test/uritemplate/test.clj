(ns uritemplate.test
  (:use [uritemplate])
  (:use [clojure.test]))

(testing "param to keyword conversion" 

  (deftest parameter
    (is (= :foo (param->keyword "{foo}"))))

  (deftest parameter-with-semicolon
    (is (= :foo (param->keyword "{:foo}"))))
  
  (deftest refuse-empty-parameter
    (is (thrown? AssertionError (param->keyword ""))))

  (deftest refuse-nil-parameter
    (is (thrown? AssertionError (param->keyword nil))))

  ;; (deftest multi-word-parameters-keeps-only-first
  ;;   (is (= :foo (param->keyword "{foo bar}"))))

  )

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
  
(testing "level 1 url templates" 
  
  (deftest one-parameter
    (let [template (url-template "http://example.com/users/{nick}")]
      (is (= "http://example.com/users/dfa"
             (template :nick "dfa")))))

  (deftest two-parameters
    (let [template (url-template "http://example.com/{nick}/{age}")]
      (is (= "http://example.com/dfa/30"
             (template :nick "dfa" :age "30"))))))
