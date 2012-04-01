(ns uritemplate.uritemplate
  (:use [uritemplate.expansions]))

(defn remove-braces [expression]
  (.substring expression 1 (dec (.length expression))))

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

;; FIXME: handle prefix + explode together
;; FIXME: ABNF rules for varname sec 2.3:
;;     variable-list =  varspec *( "," varspec )
;;     varspec       =  varname [ modifier-level4 ]
;;     varname       =  varchar *( ["."] varchar )
;;     varchar       =  ALPHA / DIGIT / "_" / pct-encoded
(defn parse-variable [variable]
  (cond
   (.contains variable "*") (parse-variable-explode variable)
   (.contains variable ":") (parse-variable-prefix variable)
   :else (parse-variable-simple variable)))

(defn parse-variable-list [variables]
  (map parse-variable (re-seq #"[^,]+" variables)))

(defn parse-as [type expression]
  "Parse a variable list yielding a seq of part of the specified type."
  (let [variable-list (parse-variable-list expression)]
    {:type type :vars (vec variable-list)}))

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

(defn expand-all [parts variables]
  (apply str (map #(expand % variables) parts)))

(defn uritemplate [template]
  (let [parts (parser (lexer template))]
    (fn [variables] (expand-all parts variables))))
