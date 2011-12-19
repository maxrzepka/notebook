(ns notebook.core
  (require [clojure.java.io :as io]))

(defn file-resource[path]
  (-> (clojure.lang.RT/baseLoader)
      (.getResourceAsStream path)
      io/reader
      line-seq))

(defn join-str[ separator coll ]
  (apply str (next (interleave (repeat separator) coll))))

(def parsers {:section (fn[tokens] {:depth (count (first tokens)) :title (join-str " " (next tokens)) :content []})
              :list-item (fn[tokens] {:depth (count (first tokens)) :title (join-str " " (next tokens)) :text []})
              :code (fn[tokens] {:language (first tokens) :name (second tokens)})})
                         

(defn tokens->node[tokens]
  (if (seq tokens)
   (let [starts { "#" :section "+" :list-item "'''" :code }
        ;; parsers {:section (fn[
        ;; ss (.substring start 0 1)
        k (first (filter #(.startsWith (first tokens) %) (keys starts)))
        
        type (starts k)
         ]
     (if type 
       (assoc ((parsers type (constantly {})) tokens) :type type )
       nil))
   {:type :empty}))

(defn detect-node [line]
  (let [tokens  (remove empty? (seq (.split line " ")))]
    (tokens->node tokens)))

(defn last-node[note]
  (last (tree-seq (comp seq :content) (comp seq :content) note)))

(defn last-node-path[note]
  (loop [note note path []]
    (if (:content note) ;; (map? (last (:content note))))
      (recur (last (:content note)) (conj path :content (dec (count (:content note)))))
      path)))

(defn new-node-path[note]
  (let [path (last-node-path

(defn append [{current :current :as note} e]
                 (let [node (detect-node e)
                       type (:type node)
                       path (last-node-path note)
                       new-path (update-in path [(dec (count path))] inc)]
                   (cond (= type current) ;;end of tag
                         (assoc note :current :empty)
                         (#{ :list-item :section } type);; deal with depth
                         (assoc (assoc-in note new-path node )
                           :current :empty)
                         (and (= type nil) (= :empty current));; start paragraph
                         (update-in note new-path conj {:type :paragraph :text [e]})
                         (or (= :empty type) (= type nil)) ;;append text to last node
                         (update-in note (conj path :text) conj e)
                         :else
                         (assoc (assoc-in note new-path node ) :current type))))

(defn transform[coll]
    (reduce append
            {:current :note :type :note :content []}
            coll))

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

)