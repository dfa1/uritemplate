(ns uritemplate.uritemplate
  (:use [uritemplate.expansions]))

(defn parse-variable-prefix [variable]
  (let [[variable prefix] (.split variable ":")
        maxlen (Integer/parseInt prefix)]
    (assert (< maxlen 10000)) ; sec 2.4.1
    (assert (> maxlen 0))     ; sec 2.4.1
    {:name variable :maxlen maxlen}))

(defn parse-variable-simple [variable]
  {:name variable})

(defn parse-variable-explode [variable]
  {:name (.substring variable 0 (dec (.length variable))) :explode true})

(defn parse-variable [variable]
  (cond
   (.contains variable "*") (parse-variable-explode variable)
   (.contains variable ":") (parse-variable-prefix variable)
   :else (parse-variable-simple variable)))

(defn parse-variable-list [variables]
  (map parse-variable (re-seq #"[^,]+" variables)))

(defn parse-as [type variable-list]
  (let [variables (parse-variable-list variable-list)]
    {:type type :vars (vec variables)}))

(defn parse-literal [token]
  {:type :literal :value token})

(defn parse-expression [expression]
  (let [variable-list (.substring expression 1 (dec (.length expression)))
        operator (.charAt variable-list 0)]
     ;; The operator characters equals ("="), comma (","), exclamation ("!"),
     ;; at sign ("@"), and pipe ("|") are reserved for future extensions.
    (case operator
      \+    (parse-as :reserved  (.substring variable-list 1)) 
      \#    (parse-as :fragment  (.substring variable-list 1))
      \.    (parse-as :dot       (.substring variable-list 1))
      \/    (parse-as :path      (.substring variable-list 1))
      \;    (parse-as :pathparam (.substring variable-list 1))
      \?    (parse-as :form      (.substring variable-list 1))
      \&    (parse-as :formcont  (.substring variable-list 1))
      (parse-as :simple    variable-list))))

(defn parse [token]
  (let [valid-expression #"\{\S+\}"]
    (if (re-matches valid-expression token)
      (parse-expression token)
      (parse-literal token))))

(defn parser [tokens]
  (flatten (map parse tokens)))

(defn lexer [template]
  (re-seq #"\{[^\{]+\}|[^{}]+" template))

(defn expand-all [parts variables]
  (apply str (map #(expand % variables) parts)))

(defn uritemplate [template]
  (let [parts (parser (lexer template))]
    (fn [variables] (expand-all parts variables))))

