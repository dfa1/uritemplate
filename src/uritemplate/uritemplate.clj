(ns uritemplate.uritemplate
  (:use [uritemplate.expansions]))

(defn tokenize [template]
  (re-seq #"\{[^\{]+\}|[^{}]+" template))

(defn parse-variable-prefix [variable]
  (let [[variable prefix] (.split variable ":")
        maxlen (Integer/parseInt prefix)]
    (assert (< maxlen 10000)) ; sec 2.4.1
    (assert (> maxlen 0))     ; sec 2.4.1
    {:name variable :maxlen maxlen}))

(defn parse-variable-explode [variable]
  {:name (.substring variable 0 (dec (.length variable))) :explode true})

(defn parse-variable-simple [variable]
  {:name variable})

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

(defn parse-type [operator]
  ;; The operator characters equals ("="), comma (","), exclamation ("!"),
  ;; at sign ("@"), and pipe ("|") are reserved for future extensions.
  (case operator
    \+     :reserved
    \#     :fragment 
    \.     :dot       
    \/     :path
    \;     :pathparam 
    \?     :form      
    \&     :formcont
    :simple))

(defn parse-expression [expression]
  (let [variable-list (.substring expression 1 (- (.length expression) 1))
        type (parse-type (.charAt variable-list 0))]
    (parse-as type (if (= type :simple) variable-list (.substring variable-list 1)))))

(defn parse-token [token]
  (let [valid-expression #"\{\S+\}"]
    (if (re-matches valid-expression token)
      (parse-expression token)
      (parse-literal token))))

(defn parse [tokens]
  (flatten (map parse-token tokens)))

(defn compile-template [template]
  (parse (tokenize template)))

(defn expand-template [compiled-template variables]
  (apply str (map #(expand % variables) compiled-template)))

(defn uritemplate [template]
  (let [compiled-template (compile-template template)]
    (fn [variables]
      (expand-template compiled-template variables))))

