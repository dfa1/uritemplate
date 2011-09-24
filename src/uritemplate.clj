(ns uritemplate)

(defn param->keyword [param]
  "Transform a string param like `{param}` into `:param`"
  (assert (not-empty param))
  (keyword (first (re-seq #"\w+" param))))

(defn keyword->param [keyword]
  "Transform a keyword into param."
  (assert (keyword? keyword))
  (str "{" (.substring (str keyword) 1) "}"))

(defn extract-params [template]
  "Returns all the url parameters of `template`."
  (assert (not-empty template))
  (map param->keyword (re-seq #"\{\w+\}" template)))

(defn url-template [template]
  "Returns a new url-template fn that accepts one keyword argument
   for each uri-parameter."
  (let [placeholders (extract-params template)]
    (fn [& more]
      (let [params (apply hash-map more)]
        (reduce #(.replace %1 (keyword->param %2) (get params %2))
              template
              (keys params))))))
