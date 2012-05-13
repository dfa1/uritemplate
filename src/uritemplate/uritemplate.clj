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

;; sec 2.3, FIXME: can accept invalid pct-encoded
(def varchar? (set (concat
                    (char-range \A \Z)
                    (char-range \a \z)
                    (char-range \0 \9)
                    [\_ \% \.])))

(defn parse-prefix [prefix]
  (let [maxlen (Integer/parseInt (apply str prefix))]
    (assert (< maxlen 10000)) ; sec 2.4.1
    (assert (> maxlen 0))     ; sec 2.4.1
    maxlen))

(defn parse-varname [varname]
  (do
    (assert (every? varchar? varname))
    (apply str varname)))

(defn parse-varspec-prefix [varspec]
  (let [[varname prefix] (split-by colon? varspec)]
    {:name (parse-varname varname)
     :maxlen (parse-prefix prefix)}))

(defn parse-varspec-explode [varspec]
  (let [varname (butlast varspec)]
    {:name (apply str varname) :explode true}))

(defn parse-varspec-simple [varname]
  {:name (apply str varname)})

(defn parse-varspec [varspec]
  (cond
   (star? (last varspec)) (parse-varspec-explode varspec)
   (some colon? varspec)  (parse-varspec-prefix  varspec)
   :else                  (parse-varspec-simple  varspec)))

;; sec 2.3 
;; variable-list =  varspec *( "," varspec )
;; varspec       =  varname [ modifier-level4 ]
;; varname       =  varchar *( ["."] varchar )
;; varchar       =  ALPHA / DIGIT / "_" / pct-encoded
(defn parse-variable-list [variable-list]
  (map parse-varspec (split-by comma? variable-list)))

(defn parse-as [type variable-list]
  (let [variables (parse-variable-list variable-list)]
    {:type type :vars (vec variables)}))

(defn parse-literal [literal]
  {:type :literal :vars [{:value (apply str literal)}]})

(def operator->type { 
   \+     :reserved 
   \#     :fragment 
   \.     :dot       
   \/     :path
   \;     :param 
   \?     :form      
   \&     :formcont
   })

(defn parse-type [variable-list]
  "Yields a pair of operator type and variable-list. Maybe consume the
operator character."
  (let [type (operator->type (first variable-list))]
    (if type
      [type    (rest variable-list)]
      [:simple variable-list])))
      
(defn- drop-braces [expression]
  "{foo,bar:1} -> foo,bar:1"
  (rest (butlast expression)))

(defn parse-expression [expression]
  (let [[type variable-list] (parse-type (drop-braces expression))]
    (parse-as type variable-list)))

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
