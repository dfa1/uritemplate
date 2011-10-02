(ns uritemplate.type4
  (:use [clojure.string :only (split join)]))

(defn url-encode [str]
  (java.net.URLEncoder/encode str "utf8"))

(defmulti expand-param class :default String)

(defmethod expand-param String [value]
  (let [splitted (split value #":" 2)] ;; it screams for destructuring
    (cond (= 1 (count splitted))
          (url-encode value)
          (= 2 (count splitted))
          (url-encode (format (format "%%.%ss" (splitted 1)) (splitted 0))))))

(defmethod expand-param clojure.lang.PersistentVector [list]
  (join "," list))

;; operator : NUL  +   .   /   ;   ?   &   # 
;; separator: "," "," "." "/" ";" "&" "&" ","
(defn expression-type [string]
  (assert (.matches string "\\{.+\\}"))
  (nth string 1))

(defmulti expand-expression expression-type :default nil)

(defmethod expand-expression nil [value]
  :simple)

(defmethod expand-expression \+ [value]
  :reserved)



