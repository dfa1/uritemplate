(ns uritemplate.uritemplate
  (:use [uritemplate.expansions]))

(defn remove-braces [expression]
  (.substring expression 1 (dec (.length expression))))

(defn parse-variable-prefix [variable]
  (let [[variable prefix] (.split variable ":")]
     {:modifier :prefix :name variable :arg prefix}))

(defn parse-variable-simple [variable]
  {:modifier :none :name variable})

(defn parse-variable-explode [variable]
  {:modifier :explode :name (.substring variable 0 (dec (.length variable)))})

(defn parse-variable [variable]
  "FIXME: handle prefix + explode together"
  (cond
   (.contains variable "*") (parse-variable-explode variable)
   (.contains variable ":") (parse-variable-prefix variable)
   :else (parse-variable-simple variable)))

(defn parse-variable-list [variables]
  (map parse-variable (re-seq #"[^,]+" variables)))

(defn parse-as [type expression]
  "Parse a variable list yielding a seq of part of the specified type."
  (let [variable-list (parse-variable-list expression)]
    (map #(assoc % :type type) variable-list)))
   
(defn parse-literal [token]
  {:type :literal :value token})

(defn parse-expression [token]
  (let [expression (remove-braces token)
        operator (.charAt expression 0)]
    (cond
     (= \+ operator) (parse-as :reserved (.substring expression 1)) 
     (= \# operator) (parse-as :fragment (.substring expression 1))
     :else (parse-as :simple expression))))

(defn parse [token]
  (let [valid-expression #"\{\S+\}"]
    (if (re-matches valid-expression token)
      (parse-expression token)
      (parse-literal token))))

(defn parser [tokens]
  (flatten (map parse tokens)))

(defn lexer [template]
  (re-seq #"\{[^/]+\}|[^{}]+" template))

(defn extract-parameters [parts]
  (remove nil? (map #(-> % :name keyword) parts)))

(defn default-parameters [keys]
  (zipmap keys (repeat nil)))

(defn merge-parameter-value [part parameters]
  (let [name (keyword (:name part))]
    (if (nil? name)
      part
      (assoc part :value (name parameters)))))

(defn expand-template [parts]
  (apply str (map expand parts)))

(defn uritemplate [template]
  (let [parts (parser (lexer template))
        wanted-parameters (default-parameters (extract-parameters parts))]
    (fn [& user-parameters]
      (let [parameters (merge wanted-parameters (apply hash-map user-parameters))]
        (expand-template (map #(merge-parameter-value % parameters) parts))))))
