(ns uritemplate.expansions)

(defn char-range [from to]
  (map char (range (int from) (inc (int to)))))

(def unreserved (set (concat (char-range \A \Z)
                             (char-range \a \z)
                             (char-range \0 \9)
                             '(\- \_ \. \~))))
(def reserved (set '(\! \* \' \( \) \;	\: \@ \& \= \+ \$ \, \/	\? \# \[ \])))

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

(defn configurable-urlencode [value pred]
  (apply str (map #(skip-pct-encode-if pred %) value)))
  
(defn urlencode [value]
  "urlencode only non-reserved characters."
  (configurable-urlencode value unreserved?))

(defn urlencode-reserved [value]
  "urlencode reserved as well as non-reserved characters."
  (configurable-urlencode value (fn [ch] (or (reserved? ch) (unreserved? ch)))))

(defn join [sep coll]
  (apply str (interpose sep coll)))

(defn render [sep value urlencoder] ; TODO: smells like multimethods
  (cond
   (nil? value) ""
   (instance? java.lang.String value) (urlencoder value)
   (number? value) (urlencoder value)
   (map? value) (urlencoder "TODO")
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

(defmulti expand :type)

(defmethod expand :literal [part variables]
  "Literal expansion."
  (:value part))

(defmethod expand :simple [part variables]
  "Simple string expansion."
  (join ","
        (remove empty?
                (map #(truncate-to (:value %) (get % :maxlen 9999))
                     (map #(assoc % :value (render "," (:value %) urlencode))
                          (map #(assoc % :value (value-of % variables)) (:vars part)))))))

(defmethod expand :reserved [part variables]
  "Reserved expansion."
  (join ","
        (remove empty?
                (map #(truncate-to (:value %) (get % :maxlen 9999))
                     (map #(assoc % :value (render "," (:value %) urlencode-reserved))
                          (map #(assoc % :value (value-of % variables)) (:vars part)))))))

(defmethod expand :fragment [part variables]
  "Fragment expansion."
  (let [expansion (join ","
                      (remove empty?
                      (map #(truncate-to (:value %) (get % :maxlen 9999))
                           (map #(assoc % :value (render "," (:value %) urlencode-reserved))
                                (map #(assoc % :value (value-of % variables)) (:vars part))))))]
    (if (empty? expansion)
      expansion
      (str "#" expansion)
    )))
