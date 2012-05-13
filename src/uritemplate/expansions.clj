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

(defn kv [kvsep [key value] encoder]
  (str (name key) kvsep (encoder value)))

;; FIXME: sorting keys in map in order to have a predicible iteration order 
(defn render-map [sep kvsep m encoder]
  "(str k1 kvsep (urlencoder v1) sep k2 kvsep (urlencoder v2) sep ...)"
  (join sep (map #(kv kvsep % encoder) (seq (sort m)))))

(defn truncate [string len]
  "Make sure string does not exceed len."
  (.substring string 0 (min len (count string))))

(defn unsupported-value [value]
  (throw
   (UnsupportedOperationException.
    (format  "unsupported type '%s'" (class value)))))

(defn with-name [name value ifemp]
  (if (empty? value)
    (str name ifemp)
    (str name "=" value)))

(defn render [cfg variable]
  (let [value    (:value variable)
        max-len  (:maxlen variable 9999)
        name     (:name variable)
        ifemp    (:ifemp cfg)
        explode? (:explode variable)
        named?   (:named cfg)
        sep      (if explode? (:sep cfg) ",")
        kvsep    (if explode? "="        ",")
        encode  (:allow cfg)]
    (cond
     (nil? value)        nil
     (string? value)     (if named?
                           (with-name name (encode (truncate value max-len)) ifemp)
                           (encode (truncate value max-len)))
     
     (number? value)     (if named?
                           (with-name name (encode (truncate (str value) max-len)) ifemp)
                           (encode (truncate (str value) max-len)))
     (map? value)       (if named?  
                          (if explode?
                            (render-map sep kvsep value encode)
                            (str name "=" (render-map sep kvsep value encode)))
                          (render-map sep kvsep value encode))
     (sequential? value) (if named?
                           (if explode?
                             (join sep (map #(str name "=" (encode %)) value))
                             (str name "=" (join sep (map encode value))))
                           (join sep (map encode value)))
     :else               (unsupported-value value))))

(defn value-of [variable variables]
  (if (:value variable)
    variable
    (assoc variable :value ((keyword (:name variable)) variables))))

(defn expander [cfg part variables]
  (map #(render cfg %) (map #(value-of % variables) (:vars part))))

;; RFC 6570
;; Appendix A
(def N str)
(def U urlencode)
(def U+R urlencode-reserved)
(def expanders
  {
   :literal  { :first ""  :sep ""  :named false :ifemp ""  :allow N   }
   :simple   { :first ""  :sep "," :named false :ifemp ""  :allow U   }
   :reserved { :first ""  :sep "," :named false :ifemp ""  :allow U+R }
   :fragment { :first "#" :sep "," :named false :ifemp ""  :allow U+R }
   :dot      { :first "." :sep "." :named false :ifemp ""  :allow U   }
   :path     { :first "/" :sep "/" :named false :ifemp ""  :allow U   }
   :param    { :first ";" :sep ";" :named true  :ifemp ""  :allow U   }
   :form     { :first "?" :sep "&" :named true  :ifemp "=" :allow U   }
   :formcont { :first "&" :sep "&" :named true  :ifemp "=" :allow U   }
   })

(defn expand [part variables]
  (let [cfg ((:type part) expanders)]
    (join-with-prefix
      (:first cfg)
      (:sep cfg)
      (expander cfg part variables))))
