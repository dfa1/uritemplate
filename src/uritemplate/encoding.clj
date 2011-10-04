(ns uritemplate.encoding)

(defn url-encode [value]
  (assert (not (nil? value)))
  (java.net.URLEncoder/encode (str value) "utf8"))

(defn reserved-url-encode [value]
  (-> value url-encode))
