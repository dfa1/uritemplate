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

(defn kv [kvsep [key value] urlencoder]
  (str key kvsep (urlencoder value)))

(defn render-map [sep kvsep m urlencoder]
  "(str k1 kvsep (urlencoder v1) sep k2 kvsep (urlencoder v2) sep ...)"
  (join sep (map #(kv kvsep % urlencoder) (seq m))))

(defn truncate [string len]
  "Make sure string does not exceed len."
  (.substring string 0 (min len (count string))))

(defn str? [obj]
  (instance? java.lang.String obj))

(defn unsupported-value [value]
  (throw
   (UnsupportedOperationException.
    (format  "unsupported type '%s'" (class value)))))

;; FIXME: too many parameters
(defn render [wanted_sep value urlencoder explode? max-len]
  (let [sep    (if explode? wanted_sep ",")
        kvsep  (if explode? "="        ",")]
    (cond
     (nil? value)        nil
     (str? value)        (urlencoder (truncate value max-len))
     (number? value)     (urlencoder (truncate (str value) max-len))
     (map? value)        (render-map sep kvsep value urlencoder)
     (sequential? value) (join sep (map urlencoder value))
     :else               (unsupported-value value))))

(defn value-of [variable variables]
  (let [name (keyword (:name variable))]
    (name variables)))

(defn expander [sep part variables urlencoder]
  (map #(render sep (:value %) urlencoder (:explode %) (:maxlen % 9999))
       (map #(assoc % :value (value-of % variables)) (:vars part))))

(defn with-name [name value ifemp]
  (if (empty? value)
    (str name ifemp)
    (str name "=" value)))

;; FIXME: too many parameters
(defn name-render [wanted_sep ifemp name value explode? max-len]
  (let [sep    (if explode? wanted_sep ",")
        kvsep  (if explode? "="        ",")]
    (cond
     (nil? value)        nil
     (str? value)        (with-name name (urlencode (truncate value max-len)) ifemp)
     (number? value)     (with-name name (urlencode (truncate (str value) max-len)) ifemp)
     (map? value)        (if explode?
                           (render-map sep kvsep value urlencode)
                           (str name "=" (render-map sep kvsep value urlencode)))
     (sequential? value) (if explode?
                           (join sep (map #(str name "=" (urlencode %)) value))
                           (str name "=" (join sep (map urlencode value))))
     :else               (unsupported-value value))))

(defn name-expander [sep ifemp part variables]
  (map #(name-render
         sep
         ifemp
         (:name %)
         (value-of % variables)
         (:explode %)
         (:maxlen % 9999)
         )
       (:vars part)))

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

(defmethod expand :pathparam [part variables]
  "Path parameter expansion. See ; in Appendix A."
  (let [first ";" sep ";" ifemp ""]
    (join-with-prefix first sep (name-expander sep ifemp part variables))))

(defmethod expand :form [part variables]
  "Form expansion. See ? in Appendix A."
  (let [first "?" sep "&" ifemp "="]
    (join-with-prefix first sep (name-expander sep ifemp part variables))))

(defmethod expand :formcont [part variables]
  "Form continuation expansion. See & in Appendix A."
  (let [first "&" sep "&" ifemp "="]
    (join-with-prefix first sep (name-expander sep ifemp part variables))))
