(ns uritemplate.expansions)


(defn char-range [from to]
    (map char (range (int from) (inc (int to)))))

(def unreserved (set (concat (char-range \A \Z)
                             (char-range \a \z)
                             (char-range \0 \9)
                             '(\- \_ \. \~))))
(def reserved (seq '(\! \* \' \( \) \;	\: \@ \& \= \+ \$ \, \/	\? \# \[ \])))

(defn pct-encode [ch] 
  (if (get unreserved ch)
    (str ch)
    (str "%" (Integer/toHexString (int ch)))))

(defn urlencode [v]
  (apply str (map pct-encode (str v))))

(defmulti expand :type)

(defmethod expand :literal [part variables]
  "Literal expansion."
  (:value part))


(defn join [sep coll]
  (apply str (interpose sep coll)))

(defn render [first sep value] ; TODO: smells like multimethods
  (cond
   (nil? value) ""
   (instance? java.lang.String value) (str first (urlencode value))
   (number? value) (str first (urlencode value))
   (map? value) (str "TODO")
   (sequential? value) (str first (join sep (map urlencode (flatten value))))
   :else (throw
          (new UnsupportedOperationException
               (str "unsupported type " (class value))))))

(defn truncate-to [str requested-len]
  (let [len (min requested-len (count str))]
    (.substring str 0 len)))

(defn value-of [variable variables]
  (let [name (keyword (:name variable))]
    (name variables)))

(defmethod expand :simple [part variables]
  "Simple string expansion."
  (join ","
        (remove empty?
                (map #(truncate-to (:value %) (get % :maxlen 9999))
                     (map #(assoc % :value (render "" "," (:value %)))
                          (map #(assoc % :value (value-of % variables)) (:vars part)))))))

(defmethod expand :reserved [part variables]
  "Reserved expansion."
  (join ","
        (remove empty?
                (map #(truncate-to (:value %) (get % :maxlen 9999))
                     (map #(assoc % :value (render "" "," (:value %)))
                          (map #(assoc % :value (value-of % variables)) (:vars part)))))))
