(ns notebook.core
  (require [clojure.java.io :as io]
           [clojure.zip :as z]))

(defn file-resource[path]
  (-> (clojure.lang.RT/baseLoader)
      (.getResourceAsStream path)
      io/reader
      line-seq))

;; only use to build title from token 
(defn join-str[ separator coll ]
  (apply str (next (interleave (repeat separator) coll))))

;; define all type of marker lines ( section , list , code )
;; A line is a marker line if starting with specific string otherwise line is a plain line
(def markers {:section "#" :list-item "+" :code "'''"})

;; For each type of marker line extract information from the line to build node map
;; only use by line->node 
;; TODO common pattern : function extract at the beginning of the line the mark
;;  tokens -> [mark tokens]
(def parsers {:section (fn[tokens] {:depth (count (first tokens)) :title (join-str " " (next tokens)) :content []})
              :list-item (fn[tokens] {:depth (count (first tokens)) :title (join-str " " (next tokens)) :text []})
              :code (fn[tokens]
                      (let [mark (:code markers)
                            ;;common pattern : extract mark from tokens (cf todo)
                            start (first tokens)
                            tokens (cond (= mark start)
                                         (next tokens)
                                         (.startsWith start mark)
                                         (cons (.substring start (.length mark)) (next tokens)))]
                            (zipmap [:content :language :name] (cons [] tokens))))})


;;
;; PB :empty tag useful at insert stage but useless after : massive removal at the
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

(defn append-node
  [loc node]
  {:pre [(not (nil? loc)) (not (nil? node))]}
  (-> loc (z/append-child node) z/down z/rightmost))


(defn parent-loc
  "Returns first  parent of loc where pred is true , returns nil if no parent found"
  [loc pred]
  (loop [loc loc]
    (cond (nil? (z/up loc)) loc ;;by default return root if nothing found
          (-> loc z/node pred) loc     
          :else (recur (z/up loc)))))
  
(defn append[loc line]
  (let [node (line->node line)]
    ;;(swank.core/break)
    (cond (and (:text node) (not (:empty node))
               (#{:paragraph :code} (:type (z/node loc)))) ;;text to append to existing text node
          (z/edit loc update-in [:content] conj (:text node))
          (and (not (:empty node)) (:text node));;create paragraph with an non-empty line
          (append-node loc
                       (-> node
                           (assoc :type :paragraph)
                           (assoc :content [(:text node)])
                           (dissoc :text)))
          ;;detect end of paragraph or end of code
          (or (and (:empty node) (= :paragraph (:type (z/node loc))))
              (= :code (:type node) (:type (z/node loc))))
          (z/up loc)
          ;;TODO handle list and section hierarchy
          (-> node :type (= :section));; handle list and section
          (let [par (parent-loc loc #(and % (= (:type %) :section)
                                               (= (:depth %) (dec (:depth node)))))]
            (append-node par node))
          (-> node :type (= :list-item));; child of upper list or closest upper section
          (if-let [par (parent-loc loc #(or (= (:type %) :section)
                                            (and % (= (:type %) :list-item)
                                                 (= (:depth %) (dec (:depth node))))))]
            (append-node par node)
            (append-node loc node))          
          (not (:empty node)) ;;just insert new child 
          (append-node loc node)
          :else ;;do nothing
          loc
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