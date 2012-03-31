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

;; FIXME: handle prefix + explode together
;; FIXME: ABNF rules for varname:
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

(defn expand-all [parts parameters]
  (apply str (map #(expand % parameters) parts)))

(defn uritemplate [template]
  (let [parts (parser (lexer template))]
    (fn [& parameters]
      (expand-all parts (apply hash-map parameters)))))

