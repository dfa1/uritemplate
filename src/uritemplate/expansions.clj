(ns uritemplate.expansions)

(defmulti expand :type)

(defmethod expand :literal [part]
  "Literal expansion."
  (:value part))

;; level1
(defmethod expand :simple [part]
  "Simple variable expansion: variable must be url-encoded"
  (java.net.URLEncoder/encode (str (:value part)) "utf8"))

;; level2
;;  var   := "value"                                    |
;;    |             hello := "Hello World!"                             |
;;    |             path  := "/foo/bar" 

;;   +  | Reserved string expansion                     (Sec 3.2.3) |
;;    |     |                                                           |
;;    |     |    {+var}                value                            |
;;    |     |    {+hello}              Hello%20World!                   |
;;    |     |    {+path}/here          /foo/bar/here                    |
;;    |     |    here?ref={+path}      here?ref=/foo/bar                |

;; (defn reserved [map]
;;   (assoc map 
;;     :subtype "reserved"
;;     :first ""
;;     :sep ","
;;     :named false
;;     :ifemp ""))



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

