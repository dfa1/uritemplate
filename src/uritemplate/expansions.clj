(ns uritemplate.expansions)

(defn char-range [from to]
  "A declarative way to declare range of characters."
  (map char (range (int from) (inc (int to)))))

(def unreserved
  "Never pct-encode these characters."
  (set (concat
        (char-range \A \Z)
        (char-range \a \z)
        (char-range \0 \9)
        '(\- \_ \. \~))))

(def reserved
  "Don't pct-encode these character on + and # expansion."
  (set '(\! \* \' \( \) \; \: \@ \& \= \+ \$ \, \/ \? \# \[ \])))

(defn unreserved? [ch]
  (contains? unreserved ch))

(defn reserved? [ch]
  (contains? reserved ch))

(defn pct-encode [ch]
  "Unconditionally percent encode input character."
  (str "%" (.toUpperCase (Integer/toHexString (int ch)))))

(defn skip-pct-encode-if [pred ch]
  (if (pred ch)
    (str ch)
    (pct-encode ch)))

(defn configurable-urlencode [value pred]
  (apply str (map #(skip-pct-encode-if pred %) (str value))))

(defn urlencode [value]
  "url-encode only non-reserved characters."
  (configurable-urlencode value unreserved?))

(defn urlencode-reserved [value]
  "url-encode reserved as well as non-reserved characters."
  (configurable-urlencode value (fn [ch] (or (reserved? ch) (unreserved? ch)))))

(defn join [sep coll]
  (apply str (interpose sep coll)))

(defn kv [kv_sep [key value] urlencoder]
  (str key kv_sep (urlencoder value)))

(defn render [wanted_sep value urlencoder explode?]
  (let [sep    (if explode? wanted_sep ",")
        kv_sep (if explode? "="        ",")]
    (cond
     (nil? value)        nil
     (instance?          java.lang.String value) (urlencoder value)
     (number? value)     (urlencoder value)
     (map? value)        (join sep (map #(kv kv_sep % urlencoder) (seq value)))
     (sequential? value) (join sep (map urlencoder value))
     :else (throw
            (new UnsupportedOperationException
                 (str "unsupported type " (class value)))))))

(defn truncate-to [str requested-len]
  (if (nil? str)
    nil
    (.substring str 0 (min requested-len (count str)))))

(defn value-of [variable variables]
  (let [name (keyword (:name variable))]
    (name variables)))
 
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
(defn expander [sep part variables urlencoder]
  (map #(truncate-to (:value %) (get % :maxlen 9999))
       (map #(assoc % :value (render sep (:value %) urlencoder (:explode %)))
            (map #(assoc % :value (value-of % variables)) (:vars part)))))

(defmulti expand :type)

(defmethod expand :literal [part variables]
  "Literal expansion."
  (:value part))

(defmethod expand :simple [part variables]
  "Simple string expansion."
  (let [sep ","]
    (join sep (remove empty? (expander sep part variables urlencode)))))

(defmethod expand :reserved [part variables]
  "Reserved expansion."
  (let [sep ","]
    (join sep (expander sep part variables urlencode-reserved))))

(defmethod expand :fragment [part variables]
  "Fragment expansion."
  (let [first "#"
        sep ","
        expansion (expander sep part variables urlencode-reserved)]
    (if (every? nil? expansion)
      ""
      (str first (join sep expansion)))))

(defmethod expand :dot [part variables]
  "Dot expansion."
  (let [first "."
        sep "."
        expansion (expander sep part variables urlencode)]
    (if (every? nil? expansion)
      ""
      (str first (join sep (remove nil? expansion))))))

(defmethod expand :path [part variables]
  "Path segment expansion."
  (let [first "/"
        sep "/"
        expansion (expander sep part variables urlencode)]
      (str first (join sep (remove nil? expansion)))))
