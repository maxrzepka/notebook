(ns notebook.core
  (require [clojure.java.io :as io]
           [clojure.zip :as z]))

(defn file-resource[path]
  (-> (clojure.lang.RT/baseLoader)
      (.getResourceAsStream path)
      io/reader
      line-seq))

(defn join-str[ separator coll ]
  (apply str (next (interleave (repeat separator) coll))))

(def markers {:section "#" :list-item "+" :code "'''"})

(def parsers {:section (fn[tokens] {:depth (count (first tokens)) :title (join-str " " (next tokens)) :content []})
              :list-item (fn[tokens] {:depth (count (first tokens)) :title (join-str " " (next tokens)) :text []})
              :code (fn[tokens] {:language (first tokens) :name (second tokens)})})

(defn line->node[line]
  (let [tokens (remove empty? (seq (.split line " ")))
        ;; detect if special line : starts with markers
        markers (zipmap (vals markers) (keys markers))
        k (if (seq tokens) (first (filter #(.startsWith (first tokens) %) (keys markers))))]
    (assoc (if k
             (let [type (markers k)]
               (assoc ((parsers type (constantly {})) tokens) :type type ))
             {:text line})
      :empty (nil? (seq tokens)))))

(defn append-node[loc node]
  (-> loc (z/append-child node) z/down z/rightmost))

(defn append[loc line]
  (let [node (line->node line)]
    (cond (and (:text node)
               (#{:paragraph :code} (:type (z/node loc)))) ;;text to append to existing text node
          (z/edit loc update-in [:content] conj (:text node))
          (and (not (:empty node)) (:text node));;create paragraph with an non-empty line
          (append-node loc (assoc node :type :paragraph))
          ;;detect end of paragraph or end of code
          (or (and (:empty node) (= :paragraph (:type (z/node loc))))
              (= :code (:type node) (:type (z/node loc))))
          (z/up loc)
          ;;TODO handle list and section hierarchy
          :else ;;just insert new child 
          (append-node loc node)          
          )))
    
(defn transform[coll]
  (z/root
    (reduce append
            (z/zipper :content (comp seq :content) #(assoc %1 :content %2)
                      {:current :note :type :note :content []})
            coll)))

(comment
  user> (n/append {:current :note :type :note :content []} "# title ")
{:current :empty, :type :note, :content [{:type :section, :depth 1, :title "title", :content []}]}
user> (def n4 (n/append {:current :note :type :note :content []} "# title "))
#'user/n4
user> (n/append n4 "")
{:current :empty, :type :note, :content [{:type :section, :depth 1, :title "title", :content []}]}
user> (n/append (n/append n4 "") "wertwe")

(def root-loc (z/zipper :content (comp seq :content) #( update-in %1 [:content] conj %2 ) {:current :note :type :note :content []} ))
user> (z/node loc-root)
{:current :note, :type :note, :content []}

user> (z/append-child loc-root {:type :section})
;; clojure.lang.PersistentArrayMap cannot be cast to java.util.Map$Entry 
;; on conj

user> (update-in {:current :note, :type :note, :content []} [:content] conj  {:type :section})
{:current :note, :type :note, :content [{:type :section}]}

(def s (n/transform ["# title" "" "line 1" "line 2" "" "'''clojure notebook" "(ns core)" "" "(println \"coucou\")" "'''" "" "line 1 of second paragraph"]))
)