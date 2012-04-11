(ns uritemplate.uritemplate
  (:use [uritemplate.expansions]))

(defn ch [wanted]
  (fn [candidate]
    (= wanted candidate)))

(def comma? (ch \,))
(def star?  (ch \*))
(def colon? (ch \:))

(defn split-by [pred coll]
  (remove #(every? pred %) (partition-by pred coll)))

(defn tokenize [template]
  (re-seq #"\{[^\{]+\}|[^{}]+" template)) ;; FIXME: try to avoid re-seq here

(defn parse-variable-prefix [variable]
  (let [[variable prefix] (split-by colon? variable)
        maxlen (Integer/parseInt (apply str prefix))]
    (assert (< maxlen 10000)) ; sec 2.4.1
    (assert (> maxlen 0))     ; sec 2.4.1
    {:name (apply str variable) :maxlen maxlen}))

(defn parse-variable-explode [variable]
  {:name (apply str (butlast variable)) :explode true})

(defn parse-variable-simple [variable]
  {:name (apply str variable)})

(defn parse-variable [variable]
  (cond
   (star? (last variable))    (parse-variable-explode variable)
   (some colon? variable)   (parse-variable-prefix variable)
   :else (parse-variable-simple variable)))

(defn parse-variable-list [variables]
  (map parse-variable (split-by comma? variables)))

(defn parse-as [type variable-list]
  (let [variables (parse-variable-list variable-list)]
    {:type type :vars (vec variables)}))

(defn parse-literal [literal]
  {:type :literal :value (apply str literal)})

(def operators->type { 
   \+     :reserved
   \#     :fragment 
   \.     :dot       
   \/     :path
   \;     :pathparam 
   \?     :form      
   \&     :formcont
   })

(defn parse-type [operator]
  (operators->type operator :simple))

(defn parse-expression [expression]
  (let [variable-list (rest (butlast expression))
        type          (parse-type (first variable-list))]
    (parse-as type (if (= type :simple) variable-list (rest variable-list)))))

(defn parse-token [token]
  (let [t (seq token)]
    (if (and (= \{ (first t)) (= \} (last t)))
      (parse-expression t)
      (parse-literal t))))

(defn parse [tokens]
  (map parse-token tokens))

(defn compile-template [template]
  (flatten (parse (tokenize template))))

(defn expand-template [compiled-template variables]
  (map #(expand % variables) compiled-template))

(defn uritemplate [template]
  (let [compiled-template (compile-template template)]
    (fn [variables]
      (apply str (expand-template compiled-template variables)))))
