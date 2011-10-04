(ns uritemplate.type1
  (:use [uritemplate.encoding]))

(defn param->keyword [param]
  "Transform a string param like `{param}` into `:param`"
  (assert (not-empty param))
  (let [words (re-seq #"\w+" param)]
    (assert (= 1 (count words)) "spaces not allowed")
    (keyword (first words))))

(defn keyword->param [keyword]
  "Transform a keyword into param."
  (assert (keyword? keyword))
  (str "{" (.substring (str keyword) 1) "}"))

(defn extract-params [template]
  "Returns all the url parameters of `template`."
  (assert (not-empty template))
  (map param->keyword (re-seq #"\{\w+\}" template)))

(defn merge-defaults
  "Merge map with a map of keys with same value, default is the empty string."
  ([map default keys]
     (merge (zipmap keys (repeat default)) map))
  ([map keys]
     (merge-defaults map "" keys)))

(defn uri-template [template]
  "Returns a new type1 uri-template fn that accepts one keyword argument
   for each uri parameter."
  (assert (not-empty template))
  (let [placeholders (extract-params template)]
    (fn [& more]
      (let [params (merge-defaults (apply hash-map more) placeholders)]
        (reduce #(.replace %1 (keyword->param %2) (url-encode (get params %2))) template (keys params))))))
