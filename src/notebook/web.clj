(ns notebook.web
  (:require [net.cgrand.enlive-html :as html]
            [somnium.congomongo :as m]
            [sandbar.stateful-session :as s])
  (:use [net.cgrand.moustache :only [app delegate]]
        [somnium.congomongo.config :only [*mongo-config*]]
        [ring.middleware.params :only [wrap-params]]
        [ring.middleware.keyword-params :only [wrap-keyword-params]]
;        [ring.middleware.reload :only [wrap-reload]]
        [ring.middleware.session :only [wrap-session]]
        [ring.middleware.file :only [wrap-file]]           
        [ring.util.response :only [response file-response redirect]]
        [ring.adapter.jetty :only [run-jetty]]))


;;
;; User Stories :
;;
;; 1) Create note : a plain text + tags
;; 2) View notes : per tag
;; 3) Update note
;; 4) authentification for any operations other than viewing
;; 4) Merge notes 
;;

;; Core Functions (TODO move to core namespace)
;; How to store notes ?
;;    1) first solution : clojure data structure save in disk (use atom , when save in disk ? )
;;    2) second solution : mongo

;; simple note structure 

;; save in disk
(def ^:dynamic *db-file* "notebook.clj" )
(def ^:dynamic *db* [])

;;TODO change to get file from classpath (resources)
(defn save-db [db filename]
  (spit filename (pr-str db)))

(defn load-db [filename]
  (read-string (slurp filename)))

;; mongo persistence

;;view/create/update functions
;; need of update ? instead only create and keep track of previous version


(defn save [collection item]
  (m/insert! collection item))

(defn fetch [collection & {:keys [id tags]}]
  (if id
    (m/fetch-one collection :where {:_id (m/object-id id)})
    (m/fetch collection :where (if (seq tags) { :tags {:$in tags}} nil))))

;; View Functions : HTML format (enlive templates)

(defn render [t]
  (apply str t))

(def render-to-response
  (comp response render))

(defn render-request [afn & args]
  (fn [req] (render-to-response (apply afn args))))

(defn prepend-attrs [att prefix]
  (fn[node] (update-in node [:attrs att] (fn[v] (str prefix v)))))

;;one HMTL page containing HTML form ,
(html/defsnippet note-view "notebook.html" [:#note ]
  [{text :text tags :tags}]
  ;;insert into input text values if existing 
  [[:p (html/nth-of-type 1)]] (html/content text)
  [[:p (html/nth-of-type 2)]] (html/content (clojure.string/join " , " tags)))

(html/defsnippet note-form "notebook.html" [:#enote]
  [{text :text tags :tags}]
  ;;insert into input text values if existing 
  [:textarea] (html/content text)
  [:input] (html/content (clojure.string/join " , " tags)))

(html/deftemplate login "login.html" []
  [[:link (html/attr= :rel "stylesheet")]] (prepend-attrs :href "/"))

;; TODO how to populate nav list : append href for each entry of navigation bar
(html/deftemplate note-edit "notebook.html" [note]
  [[:link (html/attr= :rel "stylesheet")]] (prepend-attrs :href "/")
  [:#content] (html/content (note-form note)))

(html/deftemplate note-list "notebook.html" [notes]
  [[:link (html/attr= :rel "stylesheet")]] (prepend-attrs :href "/")
  [:#content] (html/content (map note-view notes)))

(defn split-mongo-url [url]
  "Parses mongodb url from heroku, eg. mongodb://user:pass@localhost:1234/db"
  (let [matcher (re-matcher #"^.*://(.*?):(.*?)@(.*?):(\d+)/(.*)$" url)
        smatcher  (re-matcher #"^.*://(.*?):(\d+)/(.*)$" url) ] ;; Setup the regex.
    (if (.find matcher) ;; Check if it matches.
      (zipmap [:match :user :pass :host :port :db] (re-groups matcher))
      (when (.find smatcher)
        (zipmap [:match :host :port :db] (re-groups smatcher)))))) ;; Construct an options map.

(defn db-init []
  "Checks if connection, otherwise initialize."
  (when (not (m/connection? *mongo-config*)) ;; If global connection doesn't exist yet.
    (let [mongo-url (get (System/getenv) "MONGOHQ_URL" "mongodb://localhost:27017/test") ;; Heroku puts it here.
          config (split-mongo-url mongo-url)] ;; Extract options.
      (println "Initializing mongo @ " mongo-url)
      (m/mongo! :db (:db config) :host (:host config) :port (Integer. (:port config))) ;; Setup global mongo.
      (when (:user config)
        (m/authenticate (:user config) (:pass config)))))) ;; Setup u/p.

(defn wrap-always [handler & fns]
    (fn[req]
      (if (seq fns) ((first fns)))
      (handler req)))

;;
;; Add session key to initiate a session
;; TODO implement custom session storage (mongo) instead of in mem
;;
(defn authentificate[{params :params :as req}]
  (if-let [user (:user params)]
    (assoc (redirect "/notes")
      :session {:current-user user})
    ))


;; TODO collectify if only 1 note
(defn view-note[notes]
  (render-to-response (note-list notes)))

(defn save-note[{params :params :as req}]
  (view-note req (save :notes params)))

;; How introduce REST : multiple format (HMTL , JSON ) , HATEOS ( resource + links )
;; conneg function : request -> content-type -> list of accepted format in order of preference
;; available formats for each resource

;; authentification : login page + save session 
;; how to deal with note id not found

;; Description of the application
(def routes
  (app
;   (wrap-reload '[notebook.web])
   (wrap-file "resources")
   (wrap-always db-init)
   (wrap-session)
   (wrap-params)
   (wrap-keyword-params)
   ["login"] {:get (render-request login) :post authentificate}
   ["note"] {:get (render-request note-edit nil) :post save-note}
   ["notes" & tags] (view-note (fetch :notes :tags tags))
   ["note" id] {:get (view-note (fetch :notes :id id)) :post save-note }))

(defn start [ & [port & options]]
  (run-jetty (var routes) {:port (or port 8080) :join? false}))

(comment
  (w/routes {:uri "/note" :request-method :post :params {:text "test text" :tag ["misc"]}})
 )