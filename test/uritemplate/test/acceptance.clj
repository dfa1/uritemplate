(ns uritemplate.test.acceptance
  (:use [uritemplate.uritemplate])
  (:use [clojure.test]))

;; variables defined in section 3.2
(def example-variables
  {
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
   :undef      nil
   })

(defmacro expansion [template expected]
  `(let [compiled-template# (uritemplate ~template)
         got# (compiled-template# example-variables)]
     (is (= ~expected got#) (format "template: \"%s\"" ~template))))

(deftest simple-string-expansion ; section 3.2.2
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
  (expansion "{keys}"      "dot,.,semi,%3B,comma,%2C")
  (expansion "{keys*}"     "dot=.,semi=%3B,comma=%2C"))

(deftest reserved-expansion ; section 3.2.3
  (expansion "{+var}"              "value")
  (expansion "{+hello}"            "Hello%20World!")
  (expansion "{+half}"             "50%25")
  (expansion "{base}index"         "http%3A%2F%2Fexample.com%2Fhome%2Findex")
  (expansion "{+base}index"        "http://example.com/home/index")
  (expansion "O{+empty}X"          "OX")
  (expansion "O{+undef}X"          "OX")
  (expansion "{+path}/here"        "/foo/bar/here")
  (expansion "here?ref={+path}"    "here?ref=/foo/bar")
  (expansion "up{+path}{var}/here" "up/foo/barvalue/here")
  (expansion "{+x,hello,y}"        "1024,Hello%20World!,768")
  (expansion "{+path,x}/here"      "/foo/bar,1024/here")
  (expansion "{+path:6}/here"      "/foo/b/here")
  (expansion "{+list}"             "red,green,blue")
  (expansion "{+list*}"            "red,green,blue")
  (expansion "{+keys}"             "dot,.,semi,;,comma,,")
  (expansion "{+keys*}"            "dot=.,semi=;,comma=,"))

(deftest fragment-expansion ; section 3.2.4
  (expansion "{#var}"             "#value")
  (expansion "{#hello}"           "#Hello%20World!")
  (expansion "{#half}"            "#50%25")
  (expansion "foo{#empty}"        "foo#")
  (expansion "foo{#undef}"        "foo")
  (expansion "{#x,hello,y}"       "#1024,Hello%20World!,768")
  (expansion "{#path,x}/here"     "#/foo/bar,1024/here")
  (expansion "{#path:6}/here"     "#/foo/b/here")
  (expansion "{#list}"            "#red,green,blue")
  (expansion "{#list*}"           "#red,green,blue")
  (expansion "{#keys}"            "#dot,.,semi,;,comma,,")
  (expansion "{#keys*}"           "#dot=.,semi=;,comma=,"))

(deftest dot-expansion ; section 3.2.5
  (expansion "{.who}"          ".fred")
  (expansion "{.who,who}"      ".fred.fred")
  (expansion "{.half,who}"     ".50%25.fred")
  (expansion "www{.dom*}"      "www.example.com")
  (expansion "X{.var}"         "X.value")
  (expansion "X{.empty}"       "X.")
  (expansion "X{.undef}"       "X")
  (expansion "X{.var:3}"       "X.val")
  (expansion "X{.list}"        "X.red,green,blue")
  (expansion "X{.list*}"       "X.red.green.blue")
  (expansion "X{.keys}"        "X.dot,.,semi,%3B,comma,%2C")
  (expansion "X{.keys*}"       "X.dot=..semi=%3B.comma=%2C")
  (expansion "X{.empty_keys}"  "X")
  (expansion "X{.empty_keys*}" "X"))

(deftest path-segment-expansion ; section 3.2.6
  (expansion "{/who}"          "/fred")
  (expansion "{/who,who}"      "/fred/fred")
  (expansion "{/half,who}"     "/50%25/fred")
  (expansion "{/who,dub}"      "/fred/me%2Ftoo")
  (expansion "{/var}"          "/value")
  (expansion "{/var,empty}"    "/value/")
  (expansion "{/var,undef}"    "/value")
  (expansion "{/var,x}/here"   "/value/1024/here")
  (expansion "{/var:1,var}"    "/v/value")
  (expansion "{/list}"         "/red,green,blue")
  (expansion "{/list*}"        "/red/green/blue")
  (expansion "{/list*,path:4}" "/red/green/blue/%2Ffoo")
  (expansion "{/keys}"         "/dot,.,semi,%3B,comma,%2C")
  (expansion "{/keys*}"        "/dot=./semi=%3B/comma=%2C"))
