(ns uritemplate.expansions)

(defn char-range [from to]
  "A declarative way to build inclusive character ranges."
  (map char (range (int from) (inc (int to)))))

(def unreserved
  "Never pct-encode these characters."
  (set (concat
        (char-range \A \Z)
        (char-range \a \z)
        (char-range \0 \9)
        [\- \_ \. \~])))

(def reserved
  "Don't pct-encode these character on + and # expansion."
  (set [\! \* \' \( \) \; \: \@ \& \= \+ \$ \, \/ \?  \[ \] \#]))

(defn pct-encode [ch]
  "Unconditionally percent encode input character."
  (str "%" (.toUpperCase (Integer/toHexString (int ch)))))

(defn pct-encode-but [pred ch]
  "Don't percent encode if pred yields true."
  (if (pred ch)
    (str ch)
    (pct-encode ch)))

(defn urlencode-but [pred string]
  "Url encode string, skipping a character when pred yields true."
  (apply str (map #(pct-encode-but pred %) string)))

(defn in? [& colls]
  "Factory (or (contains? coll1 key) (contains? coll2 key) ..etc)."
  (fn [key]
    (some #(contains? % key) colls)))

(defn urlencode [string]
  "url encode all but unreserved characters."
  (urlencode-but (in? unreserved) string))

(defn urlencode-reserved [string]
  "url encode all but reserved and unreserved characters."
  (urlencode-but (in? reserved unreserved) string))

(defn join [sep coll]
  "Joins coll with sep."
  (apply str (interpose sep coll)))

(defn join-with-prefix [prefix sep coll]
  "As join but prepending prefix iff when result is not empty."
  (let [filtered-coll (remove nil? coll)]
    (if (= 0 (count filtered-coll))
      ""
      (str prefix (join sep filtered-coll)))))

(defn kv [kv_sep [key value] urlencoder]
  (str key kv_sep (urlencoder value)))

(defn truncate [string len]
  "Make sure string does not exceed len."
  (.substring string 0 (min len (count string))))

(defn str? [obj]
  (instance? java.lang.String obj))

(defn render [wanted_sep value urlencoder explode? max-len]
  (let [sep    (if explode? wanted_sep ",")
        kv_sep (if explode? "="        ",")]
    (cond
     (nil? value)        nil
     (str? value)        (urlencoder (truncate value max-len))
     (number? value)     (urlencoder (truncate (str value) max-len))
     (map? value)        (join sep (map #(kv kv_sep % urlencoder) (seq value)))
     (sequential? value) (join sep (map urlencoder value))
     :else (throw
            (new UnsupportedOperationException
                 (str "unsupported type " (class value)))))))

(defn value-of [variable variables]
  (let [name (keyword (:name variable))]
    (name variables)))

;; FIXME: this function needs some love
(defn expander [sep part variables urlencoder]
  (map :value
       (map #(assoc % :value (render sep (:value %) urlencoder (:explode %) (get % :maxlen 9999)))
            (map #(assoc % :value (value-of % variables)) (:vars part)))))

;; RFC 6570
;; Appendix A
;;
;; .------------------------------------------------------------------.
;; |          NUL     +      .       /       ;      ?      &      #   |
;; |------------------------------------------------------------------|
;; | first |  ""     ""     "."     "/"     ";"    "?"    "&"    "#"  |
;; | sep   |  ","    ","    "."     "/"     ";"    "&"    "&"    ","  |
;; | named | false  false  false   false   true   true   true   false |
;; | ifemp |  ""     ""     ""      ""      ""     "="    "="    ""   |
;; | allow |   U     U+R     U       U       U      U      U     U+R  |
;; `------------------------------------------------------------------'
(defmulti expand :type)

(defmethod expand :literal [part variables]
  "Literal expansion."
  (:value part))

(defmethod expand :simple [part variables]
  "Simple string expansion. See NUL in Appendix A."
  (let [sep ","]
    (join sep (remove empty? (expander sep part variables urlencode)))))

(defmethod expand :reserved [part variables]
  "Reserved expansion. See + in Appendix A."
  (let [sep ","]
    (join sep (expander sep part variables urlencode-reserved))))

(defmethod expand :fragment [part variables]
  "Fragment expansion. See # in Appendix A."
  (let [first "#" sep "," urlencoder urlencode-reserved]
    (join-with-prefix first sep (expander sep part variables urlencoder))))

(defmethod expand :dot [part variables]
  "Dot expansion. See . in Appendix A."
  (let [first "." sep "." urlencoder urlencode]
    (join-with-prefix first sep (expander sep part variables urlencoder))))

(defmethod expand :path [part variables]
  "Path segment expansion. See / in Appendix A."
  (let [first "/" sep "/" urlencoder urlencode]
    (join-with-prefix first sep (expander sep part variables urlencoder))))

