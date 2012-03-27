(ns uritemplate.encoding)

(defn url-encode [value]
  "A thin wrapper around standard Java urlencoder that always uses
utf8 on the string representation of the specified value."
  (assert (not (nil? value)))
  (java.net.URLEncoder/encode (str value) "utf8"))
