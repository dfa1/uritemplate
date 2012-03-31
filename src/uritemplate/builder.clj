(ns uritemplate.builder
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

(defn with-type [type]
  "Function factory for setting :type."
  (fn [map]
    (assoc map :type type)))

(defn parse-as [type expression]
  (let [variable-list (parse-variable-list expression)]
    (map (with-type type) variable-list)))
   
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
  (let [key (keyword (:name part))
        value (key parameters)]
   (assoc part :value value)))

(merge-parameter-value {:name "name" :foo 2 :bar 1} {:mario "rossi" :domain "google.com" :name "asd"})

(defn uritemplate [template]
  (let [parts (parser (lexer template))
        default-parameters (default-parameters (extract-parameters parts))]
    (fn [& kvs]
      (let [parameters (merge default-parameters (apply hash-map kvs))
            valued-parts (map #(merge-parameter-value % parameters) parts)]
        (apply str (map expand valued-parts)))
    )))

  (comment (default-parameters (extract-parameters (parser (lexer  "http://www.{domain}/{+context}/{#anchor}")))))

