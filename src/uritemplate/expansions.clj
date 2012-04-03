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

(defn unexplode [coll sep urlencoder]
  (join sep (map urlencoder (flatten (seq coll)))))

(defn kv [[key value] urlencoder]
  (str key "=" (urlencoder value)))

(defn explode [coll sep urlencoder]
  (join sep (map #(kv % urlencoder) (seq coll))))

(defn render [sep value urlencoder explode?]
  (let [exploder (if explode? explode unexplode)]
    (cond
     (nil? value)        nil
     (instance?          java.lang.String value) (urlencoder value)
     (number? value)     (urlencoder value)
     (map? value)        (exploder value sep urlencoder)
     (sequential? value) (unexplode value sep urlencoder)
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
(defn expander [part variables urlencoder]
  (map #(truncate-to (:value %) (get % :maxlen 9999))
       (map #(assoc % :value (render "," (:value %) urlencoder (:explode %)))
            (map #(assoc % :value (value-of % variables)) (:vars part)))))

(defmulti expand :type)

(defmethod expand :literal [part variables]
  "Literal expansion."
  (:value part))

(defmethod expand :simple [part variables]
  "Simple string expansion."
  (join "," (remove empty? (expander part variables urlencode))))

(defmethod expand :reserved [part variables]
  "Reserved expansion."
  (join "," (remove empty? (expander part variables urlencode-reserved))))

(defmethod expand :fragment [part variables]
  "Fragment expansion."
  (let [expansion (expander part variables urlencode-reserved)]
    (if (every? nil? expansion)
      ""
      (str "#" (join "," (remove empty? expansion))))))
