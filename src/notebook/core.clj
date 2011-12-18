(ns notebook.core
  (require [clojure.java.io :as io]))

(defn file-resource[path]
  (-> (clojure.lang.RT/baseLoader)
      (.getResourceAsStream path)
      io/reader
      line-seq))

(def parsers {:section (fn[tokens] {:depth (count (first tokens)) :title (apply str (interleave "" (next tokens)))})
              :list-item (fn[tokens] {:depth (count (first tokens)) :title (apply str (interleave "" (next tokens)))})
              :code (fn[tokens] {:language (first tokens) :name (second tokens)})})
                         

(defn tokens->type[tokens] 
  (let [starts { "#" :section "+" :list-item "'''" :code }
        ;; parsers {:section (fn[
        ;; ss (.substring start 0 1)
        k (first (filter #(.startsWith (first tokens) %) (keys starts)))
        
        type (starts k :paragraph)
        ]
    (assoc ((parsers type (constantly {})) tokens) :type type )))

(defn detect-type [line]
  (let [tokens  (remove empty? (seq (.split line " ")))
        sign (second re-find #"\s*([^\s]+)\s+" line)]
    (tokens->type tokens)))

(defn transform[coll]
  (let [append (fn[a e]
                 )]
    (reduce append
            {:type :note :content []}
            coll)))
