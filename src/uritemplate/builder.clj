(defn join [coll]
  (apply str coll))

(join (butlast "123"))
(join (rest "123"))
(join (interpose "," "123"))

(defn lexer [template] 
  (re-seq #"\{[^/]+\}|[^{}]+" template))

(lexer nil)
(lexer "")
(lexer "www.example.com")
(lexer "www{.dom*}/context/index.php?param=2")
(lexer "www{/context/index.php?param=2") ; FIXME: error unmatched {
(lexer "{foo}/{bar}") 


(defn parse-variable-explode [variable]
  [:explode (join (butlast variable))]) 

(parse-variable-explode "asd*")

(defn parse-variable-prefix [variable]
  (let [[variable prefix] (.split variable ":")]
    [:prefix variable prefix]))

(parse-variable-prefix "asd:123")

(defn parse-variable [variable]
  (cond
   (.contains variable "*") (parse-variable-explode variable)
   (.contains variable ":") (parse-variable-prefix variable)
   :else [:simple variable]))

; FIXME: handle prefix + explode together 
(defn parse-variable-list [variables]
  (map parse-variable (re-seq #"[^,]+" variables)))

(parse-variable-list "a")
(parse-variable-list "a,b")
(parse-variable-list "a,b*")
(parse-variable-list "a,b:12")

(defn remove-braces [expression]
  (.substring expression 1 (dec (.length expression))))

(remove-braces "{2}")

(defn parse-expression [token]
  (let [expression (remove-braces token)
        operator (.charAt expression 0)]
    (cond
     (= \+ operator) (parse-variable-list (.substring expression 1))
     (= \# operator) (parse-variable-list (.substring expression 1))
     :else (parse-variable-list expression))))

(parse-expression "{+foo*,bar}")

(defn parse-literal [token]
  [:literal token])

(defn parse [token]
  (let [valid-expression #"\{\S+\}"]
    (if (re-matches valid-expression token)
      (parse-expression token)
      (parse-literal token))))

(parse "{+er}")

(defn parser [tokens]
  (map parse tokens))

(parser (lexer "http://example.com/{+hello}/{#x,y}"))


