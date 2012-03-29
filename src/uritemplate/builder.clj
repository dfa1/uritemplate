(ns uritemplate.builder)

(defn remove-braces [expression]
  (.substring expression 1 (dec (.length expression))))

(defn parse-variable-prefix [variable]
  (let [[variable prefix] (.split variable ":")]
     {:type :prefix :name variable :arg prefix}))

(defn parse-variable-simple [variable]
  {:type :simple :name variable})

(defn parse-variable-explode [variable]
  {:type :explode :name (.substring variable 0 (dec (.length variable)))}) 

(defn parse-variable [variable]
  "FIXME: handle prefix + explode together"
  (cond
   (.contains variable "*") (parse-variable-explode variable)
   (.contains variable ":") (parse-variable-prefix variable)
   :else (parse-variable-simple variable)))

(defn parse-variable-list [variables]
  (map parse-variable (re-seq #"[^,]+" variables)))

(defn parse-literal [token]
  {:type :literal :value token})

(defn parse-expression [token]
  (let [expression (remove-braces token)
        operator (.charAt expression 0)]
    (cond
     (= \+ operator) (parse-variable-list (.substring expression 1))) 
     (= \# operator) (parse-variable-list (.substring expression 1))
     :else (parse-variable-list expression)))

(defn parse [token]
  (let [valid-expression #"\{\S+\}"]
    (if (re-matches valid-expression token)
      (parse-expression token)
      (parse-literal token))))

(defn parser [tokens]
  (map parse tokens))

(defn lexer [template]
  "FIXME: must return error on unmatched {"
  (re-seq #"\{[^/]+\}|[^{}]+" template))

(defn builder [template]
  (parser (lexer template)))

;; (defn simple [map]
;;   (assoc map
;;     :subtype "simple"
;;     :first ""
;;     :sep ","
;;     :named false
;;     :ifemp ""))

;; (defn reserved [map]
;;   (assoc map 
;;     :subtype "reserved"
;;     :first ""
;;     :sep ","
;;     :named false
;;     :ifemp ""))
