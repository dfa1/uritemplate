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

(defn parse-variable-list [variables]
  (re-seq #"[^,]+" variables)) ; FIXME: handle prefix + explode together 

(parse-variable-list "a")
(parse-variable-list "a,b")
(parse-variable-list "a,b*")
(parse-variable-list "a,b:12")

(defn parse-variable-explode [variable]
  [:explode (join (butlast variable))]) 

(parse-variable-explode "asd*")

(defn parse-variable-prefix [variable]
  (let [[variable prefix] (.split variable ":")]
    [:prefix variable (Integer/parseInt prefix)]))

(parse-variable-prefix "asd:123")

(defn parse-variable [variable]
  (cond
   (.contains variable "*") (parse-variable-explode variable)
   (.coùntains variable ":") (parse-variable-prefix variable)
   :else [:simple variable]))

(

(defn parse-expression [token]
  
  [:expression token])

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

(parser (lexer "http://example.com/{+hello}/{;x,y}"))


