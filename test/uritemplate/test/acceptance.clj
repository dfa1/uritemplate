(ns uritemplate.test.acceptance
  (:use [uritemplate.uritemplate])
  (:use [clojure.test])
  (:use [clojure.data.json]))

(defn- run-testcase [level variables testcase]
  (let [template (first testcase)
        expected (second testcase)
        compiled-template (uritemplate template)
        got (compiled-template variables)]
    (println (format "testing template '%s' is '%s'" template expected)) 
    (is (= expected got)
        (format "template level %s: '%s', expected: '%s', got '%s'" level template expected got))))

(defn- run-testcases-by-level [data]
  (let [variables (:variables data)]
    (doall
     (map #(run-testcase (:level data) variables %) (:testcases data)))))

(defn- load-specs []
  (clojure.java.io/reader
   (clojure.java.io/resource "uritemplate/test/specs.json")))

(deftest rfc-specs
  (doall
   (map run-testcases-by-level
        (vals (read-json (load-specs))))))
