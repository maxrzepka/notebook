# Web Development and REST 

## Introduction

Explain Noir based on compojure, what kind of problems Noir are resolving. Compare it with Moustache or how to leverage Moustache to get same features as Noir.
In next step, find pratical ways to do RESTful application ie :

+ Resource oriented where HTML output format is just one of the format supported
+ Content Negociation 
+ Web Machine : what brings us ?
+ HATEOS : create / publish resources links

## Noir Framework

## Restful params / response middleware

Based on [ngrunwald / ring-middleware-format](https://github.com/ngrunwald/ring-middleware-format) library , how enhance it to support content negociation.
Example to use HTML , XML templates. Here the two central functions :

'''clojure
(defn wrap-format-response
  "Wraps a handler such that responses body to requests are formatted to the right format.
:predicate is a predicate taking the request and response as arguments to test if serialization should be used.
:encoder specifies a fn taking the body as sole argument and giving back an encoded string.
:type allows to specify a Content-Type for the encoded string.
:charset can be either a string representing a valid charset or a fn taking the req as argument and returning a valid charset (utf-8 is strongly suggested)."
  [handler & {:keys [predicate encoder type charset]}]
  (fn [req]
    (let [{:keys [headers body] :as response} (handler req)]
      (if (predicate req response)
        (let [char-enc (if (string? charset) charset (charset req))
              body-string (encoder body)
              body* (.getBytes body-string char-enc)
              body-length (count body*)]
          (-> response
              (assoc :body (io/input-stream body*))
              (res/content-type (str type "; charset=" char-enc))
              (res/header "Content-Length" body-length)))
        response))))

'''

'''clojure
(defn wrap-format-params
    "Wraps a handler such that requests body are deserialized from to the right format, added in a :body-params key and merged in :params. It takes 3 args:
:predicate is a predicate taking the request as sole argument to test if deserialization should be used.
:decoder specifies a fn taking the body String as sole argument and giving back a hash-map.
:charset can be either a string representing a valid charset or a fn taking the req as argument and returning a valid charset."
  [handler & {:keys [predicate decoder charset]}]
  (fn [req]
    (if (predicate req)
      (let [body (:body req)
            char-enc (if (string? charset) charset (charset req))
            bstr (slurp body :encoding char-enc)
            fmt-params (decoder bstr)
            req* (assoc req
                   :body-params fmt-params
                   :params (merge (:params req) fmt-params))]
        (handler req*))
      (handler req))))

'''