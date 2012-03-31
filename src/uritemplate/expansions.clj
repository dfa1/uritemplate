(ns uritemplate.expansions)

(defn urlencode [v]
  (java.net.URLEncoder/encode (str v) "utf8"))

(defn render [first sep value] ; TODO: smells like multimethods
  (cond
   (nil? value) ""
   (instance? java.lang.String value) (str first (urlencode value))
   (number? value) (str first (urlencode value))
   (map? value) (str "TODO")
   (or (vector? value) (list? value)) (str first
                      (apply str
                             (interpose sep
                                        (map urlencode (flatten value)))))
   :else (throw
          (new UnsupportedOperationException
               (str "unsupported type " (class value))))))

(defn value-of [parameter parameters]
  (let [name (keyword (:name parameter))]
    (name parameters)))

(defmulti expand :type)

(defmethod expand :literal [part parameters]
  "Literal expansion."
  (:value part))

(defmethod expand :simple [part parameters]
  "Simple string expansion."
  (apply str 
         (interpose ","
                    (remove empty?
                            (map #(render "" "," %)
                                 (map #(value-of % parameters) (:vars part)))))))

;; (expand {:type :simple, :vars [{:modifier :none, :name "foo"} {:modifier :none, :name "bar"}]} {:foo "foo value" :bar "bar value"})

  
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
