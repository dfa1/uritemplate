(ns uritemplate.expansions)

(defmulti expand :type)

(defmethod expand :literal [part variables]
  "Literal expansion."
  (:value part))

(defn urlencode [v]
  (java.net.URLEncoder/encode (str v) "utf8"))

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

  
;;   +  | Reserved string expansion                     (Sec 3.2.3) |
;;    |     |                                                           |
;;    |     |    {+var}                value                            |
;;    |     |    {+hello}              Hello%20World!                   |
;;    |     |    {+path}/here          /foo/bar/here                    |
;;    |     |    here?ref={+path}      here?ref=/foo/bar                |

(defmethod expand :reserved [part parameters]
  "TODO")


;;    |-----+-----------------------------------------------------------|
;;    |  #  | Fragment expansion, crosshatch-prefixed       (Sec 3.2.4) |
;;    |     |                                                           |
;;    |     |    X{#var}               X#value                          |
;; |     |    X{#hello}             X#Hello%20World!

;; reserved-string-expand
;; fragment-expand

;; multiple-expand

;; map?{x,y}             map?1024,768                     |
;; {x,hello,y}           1024,Hello%20World%21,768


;; multiple-reseverd-expand
;;    {+x,hello,y}          1024,Hello%20World!,768          |
;;    |     |    {+path,x}/here        /foo/bar,1024/here          


;; {#x,hello,y}          #1024,Hello%20World!,768         |
;; {#path,x}/here        #/foo/bar,1024/here
