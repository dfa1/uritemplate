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
   (.toUpperCase (str "%" (Integer/toHexString (int ch)))))

(defn skip-pct-encode-if [pred ch]
  (if (pred ch)
    (str ch)
    (pct-encode ch)))

(defn configurable-urlencode [value pred]'
  (apply str (map #(skip-pct-encode-if pred %) value)))
  
(defn urlencode [value]
  "urlencode only non-reserved characters."
  (configurable-urlencode value unreserved?))

(defn urlencode-reserved [value]
  "urlencode reserved as well as non-reserved characters."
  (configurable-urlencode value (fn [ch] (or (reserved? ch) (unreserved? ch)))))

(defn join [sep coll]
  (apply str (interpose sep coll)))

(defn unexplode [coll]
  (flatten (seq coll)))

(defn kv [[key value] urlencoder]
  (vector key "=" (urlencoder value)))

(defn explode [coll urlencoder]
  (flatten (interpose "," (map #(kv % urlencoder) (seq coll)))))

(defn render [sep value urlencoder] 
  (cond
   (nil? value)        ""
   (instance?          java.lang.String value) (urlencoder value)
   (number? value)     (urlencoder value)
   (map? value)        (join sep (map urlencoder (unexplode value)))
   (sequential? value) (join sep (map urlencoder value))
   :else (throw
          (new UnsupportedOperationException
               (str "unsupported type " (class value))))))

(defn truncate-to [str requested-len]
  (let [len (min requested-len (count str))]
    (.substring str 0 len)))

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

(defn- configurable-expander [part variables urlencoder]
  (join ","
        (remove empty?
                (map #(truncate-to (:value %) (get % :maxlen 9999))
                     (map #(assoc % :value (render "," (:value %) urlencoder))
                          (map #(assoc % :value (value-of % variables)) (:vars part)))))))

(defmulti expand :type)

(defmethod expand :literal [part variables]
  "Literal expansion."
  (:value part))

(defmethod expand :simple [part variables]
  "Simple string expansion."
  (configurable-expander part variables urlencode))

(defmethod expand :reserved [part variables]
  "Reserved expansion."
  (configurable-expander part variables urlencode-reserved))

(defmethod expand :fragment [part variables]
  "Fragment expansion."
  (let [expansion (configurable-expander part variables urlencode-reserved)]
    (if (empty? expansion)
      expansion
      (str "#" expansion))))
