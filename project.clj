(defproject notebook/notebook "0.1.0" 
  :min-lein-version "2.0.0"
  :profiles {:dev {:dependencies [[kerodon "0.0.4"]]}}
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [ring "1.0.2"]
                 [congomongo "0.1.8"]
                 [org.clojars.sritchie09/enlive "1.2.0-alpha1"]
                 [net.cgrand/moustache "1.1.0"]]
  :description "FIXME: write description")
