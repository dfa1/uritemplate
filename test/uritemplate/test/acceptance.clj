(ns uritemplate.test.acceptance
  (:use [uritemplate.uritemplate])
  (:use [clojure.test]))

(defn- expansion [template expansion]
  (let [compiled-template (uritemplate template)]
    (is (= expansion (compiled-template
                  :dom        "example.com"
                  :dub        "me/too"
                  :foo        "That’s right!"
                  :hello      "Hello World!"
                  :half       "50%"
                  :var        "value"
                  :who        "fred"
                  :base       "http://example.com/home/"
                  :path       "/foo/bar"
                  :list       [ "red" "green" "blue" ]
                  :keys       {"semi" ";" "dot" "." "comma" ","}
                  :v          "6"
                  :x          "1024"
                  :y          "768"
                  :empty      ""
                  :empty_keys []
                  :undef      nil)))))


(deftest simple-string-expansion
  (expansion "{var}"       "value")
  (expansion "{hello}"     "Hello%20World%21")
  (expansion "{half}"      "50%25")
  (expansion "O{empty}X"   "OX")
  (expansion "O{undef}X"   "OX")
  (expansion "{x,y}"       "1024,768")
  (expansion "{x,hello,y}" "1024,Hello%20World%21,768")
  (expansion "?{x,empty}"  "?1024")
  (expansion "?{x,undef}"  "?1024")
  (expansion "?{undef,y}"  "?768")
  (expansion "{var:3}"     "val")
  (expansion "{var:30}"    "value")
  (expansion "{list}"      "red,green,blue")
  (expansion "{list*}"     "red,green,blue")
  (expansion "{keys}"      "semi,%3B,dot,.,comma,%2C")
  (expansion "{keys*}"     "semi=%3B,dot=.,comma=%2C"))
  
