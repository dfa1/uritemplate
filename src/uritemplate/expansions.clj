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

(defn truncate-to [str requested-len]
  (if (nil? str)
    nil
    (.substring str 0 (min requested-len (count str)))))

(defn str? [obj]
  (instance? java.lang.String obj))

(defn render [wanted_sep value urlencoder explode? max-len]
  (let [sep    (if explode? wanted_sep ",")
        kv_sep (if explode? "="        ",")]
    (cond
     (nil? value)        nil
     (str? value)        (urlencoder (truncate-to value max-len))
     (number? value)     (urlencoder (truncate-to value max-len))
     (map? value)        (join sep (map #(kv kv_sep % urlencoder) (seq value)))
     (sequential? value) (join sep (map urlencoder value))
     :else (throw
            (new UnsupportedOperationException
                 (str "unsupported type " (class value)))))))

(defn value-of [variable variables]
  (let [name (keyword (:name variable))]
    (name variables)))

(defn expander [sep part variables urlencoder]
  (map :value
       (map #(assoc % :value (render sep (:value %) urlencoder (:explode %) (get % :maxlen 9999)))
            (map #(assoc % :value (value-of % variables)) (:vars part)))))


(defn prepend-prefix [first sep expansion]
  (if (every? nil? expansion)
    ""
    (str first (join sep (remove nil? expansion)))))

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
;;
;; NUL is mapped to :simple

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
  (let [first "#" sep "," urlencoder urlencode-reserved]
    (prepend-prefix first sep (expander sep part variables urlencoder))))

(defmethod expand :dot [part variables]
  "Dot expansion."
  (let [first "." sep "." urlencoder urlencode]
    (prepend-prefix first sep (expander sep part variables urlencoder))))

(defmethod expand :path [part variables]
  "Path segment expansion."
  (let [first "/" sep "/" urlencoder urlencode]
    (prepend-prefix first sep (expander sep part variables urlencoder))))

