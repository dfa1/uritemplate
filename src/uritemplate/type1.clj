(ns uritemplate.type1)

(defn keyword->param [keyword]
  "Transform a keyword into param."
  (assert (keyword? keyword))
  (str "{" (.substring (str keyword) 1) "}"))

(defn expand-value [value]
  (java.net.URLEncoder/encode (str value) "utf8"))

(defn uri-template [template]
  "Returns a new type1 uri-template fn that accepts one keyword argument
   for each uri parameter."
  (assert (not-empty template))
  (fn [& more]
    (let [params (apply hash-map more)]
      (reduce #(.replace %1 (keyword->param %2) (expand-value (%2 params)))
              template
              (keys params)))))
 
