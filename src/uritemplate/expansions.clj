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
  "Factory for (or (contains? coll1 key) (contains? coll2 key) ..etc)."
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

(defn kv [kvsep [key value] encoder]
  (str (name key) kvsep (encoder value)))

;; FIXME: sorting keys in map in order to have a predicible iteration order 
(defn expand-map [sep kvsep m encoder]
  "(str k1 kvsep (encoder v1) sep k2 kvsep (encoder v2) sep ...)"
  (join sep (map #(kv kvsep % encoder) (seq (sort m)))))

(defn truncate-to [max-len]
  "Factory for a function that truncates strings to the specified max-len."
  (fn [string]
    (let [stream (seq string)
          len (min max-len (count stream))]
      (apply str (take len stream)))))

(defn with-name [name value ifemp]
  (if (empty? value)
    (str name ifemp)
    (str name "=" value)))

(defn expand-variable [variable cfg]
  "I'm big and bad."
  (let [value       (:value variable)
        truncate    (truncate-to (:maxlen variable 9999))
        name        (:name variable)
        ifemp       (:ifemp cfg)
        explode?    (:explode variable)
        named?      (:named cfg)
        sep         (if explode? (:sep cfg) ",")
        kvsep       (if explode? "="        ",")
        encode      (:allow cfg)]
  (cond
   (nil? value)        nil
   (string? value)     (let [expanded (encode (truncate value))]
                         (if named?
                           (with-name name expanded ifemp)
                           expanded))
   (number? value)     (let [expanded (encode (truncate (str value)))]
                         (if named?
                           (with-name name expanded ifemp)
                           expanded))
   (map? value)        (let [expanded (expand-map sep kvsep value encode)]
                         (if named?  
                           (if explode?
                             expanded
                             (str name "=" expanded))
                           expanded))
   (sequential? value) (if named?
                         (if explode?
                           (join sep (map #(str name "=" (encode %)) value))
                           (str name "=" (join sep (map encode value))))
                         (join sep (map encode value)))
   :else                (throw
                         (UnsupportedOperationException.
                          (format  "unsupported type '%s'" (class value)))))))

(defn value-of [variable variables]
  "Join on variable name with variables."
  (if (:value variable)
    variable
    (assoc variable :value ((keyword (:name variable)) variables))))

(defn resolve-variables [part variables]
  (map #(value-of % variables) (:vars part)))

(defn expand-variables [part variables cfg]
  (map #(expand-variable % cfg) (resolve-variables part variables)))

(def expanders
  "As defined by Appending A of RFC 6570"
  (let [N str U urlencode U+R urlencode-reserved] {
     :literal  { :first ""  :sep ""  :named false :ifemp ""  :allow N   }
     :simple   { :first ""  :sep "," :named false :ifemp ""  :allow U   }
     :reserved { :first ""  :sep "," :named false :ifemp ""  :allow U+R }
     :fragment { :first "#" :sep "," :named false :ifemp ""  :allow U+R }
     :dot      { :first "." :sep "." :named false :ifemp ""  :allow U   }
     :path     { :first "/" :sep "/" :named false :ifemp ""  :allow U   }
     :param    { :first ";" :sep ";" :named true  :ifemp ""  :allow U   }
     :form     { :first "?" :sep "&" :named true  :ifemp "=" :allow U   }
     :formcont { :first "&" :sep "&" :named true  :ifemp "=" :allow U   }
     }))

(defn expand [part variables]
  (let [cfg ((:type part) expanders)]
    (join-with-prefix
      (:first cfg)
      (:sep cfg)
      (expand-variables part variables cfg))))
