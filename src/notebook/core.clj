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
(def markers {:section "#" :list-item "+" :code "'''" :quote ">" :tag "%"})

;; For each type of marker line extract information from the line to build node map
;; only use by line->node 
;; TODO common pattern : function extract at the beginning of the line the mark
;;  tokens -> [mark tokens]
(def parsers {:section (fn[tokens] {:depth (count (first tokens)) :title (join-str " " (next tokens)) :content []})
              :list-item (fn[tokens] {:depth (count (first tokens)) :title (join-str " " (next tokens)) :text []})
              :tag (fn[tokens] {:tags (rest tokens)})
              :quote (fn[tokens] {:text (join-str " " (rest tokens)) :content [(join-str " " (rest tokens))]}) ;;TODO trim quote at the beginning
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
;; PB :empty entry useful at insert stage but useless after : massive removal at the
(defn line->node
  "converts a plain text line to a node (ie a map) ,
   line without special marks gives {:text line}
   :empty detects empty line
   "
  [line]
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
  "Append node as rightmost children of loc and move to this node"
  [loc node]
  {:pre [(not (nil? loc)) (not (nil? node))]}
  (-> loc (z/append-child node) z/down z/rightmost))


(defn parent-loc
  "Returns first  parent of loc where pred is true , returns root if no parent found"
  [loc pred]
  (loop [loc loc]
    (cond (nil? (z/up loc)) loc ;;by default return root if nothing found
          (-> loc z/node pred) loc     
          :else (recur (z/up loc)))))
  
(defn append
  "appends node to the zipper at location loc based on kind of
   Contains dispatch logic to insert node in zipper
  "
  [loc line]
  (let [node (line->node line)
        current-type (-> loc z/node :type)]
;;    (swank.core/break)
    (cond ;;text to append to current node
     (or (and (= :paragraph current-type)
              (not (:empty node)) ;; append to paragraph if non empty plain line
              (nil? (:type node)))
         (= :quote (:type node) current-type) ;;append to quote if new line is a quote also
         (and (= :code (-> loc z/node node)) ;;append to code node if not end of code
              (not= :code (:type node))))
     (z/edit loc update-in [:content] conj (:text node))
     (and (not (:empty node)) (:text node));;create paragraph with an non-empty line
     (append-node (if (= :quote current-type) (z/up loc) loc)
                  (-> node
                      (assoc :type (:type node :paragraph))
                      (assoc :content [(:text node)])
                      (dissoc :text)))
     ;;detect end of paragraph , code or quote
     (or (and (:empty node)
              (= :paragraph (:type (z/node loc))))
         ;; closing/new code mark 
         (= :code (:type node) (:type (z/node loc))))
     (z/up loc)
     (= :section (:type node));; handle list and section
     (let [par (parent-loc loc #(and % (= (:type %) :section)
                                     (= (:depth %) (dec (:depth node)))))]
       (append-node par node))
     (= :list-item (:type node));; child of upper list or closest upper section
     (if-let [par (parent-loc loc #(or (= (:type %) :section)
                                       (and % (= (:type %) :list-item)
                                            (= (:depth %) (dec (:depth node))))))]
       (append-node par node)
       (append-node loc node))
     (= :tag (:type node)) ;; append tags information to map
     (z/edit loc assoc :tags (:tags node))
     (not (:empty node)) ;;just insert new child
     (if (:content node)
       (append-node loc node)
       (-> loc (append-node node) z/up)) ;;append to parent if current node do not accept children
     :else ;;do nothing
     loc)))
    
(defn transform
  "From a sequence of lines in markdown format returns a map where :content stores text or nodes , :type "
  [lines]
  (z/root
    (reduce append
            (z/zipper :content (comp seq :content) #(assoc %1 :content %2)
                      {:type :note :content []})
            lines)))

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