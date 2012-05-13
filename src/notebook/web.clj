(ns notebook.web
  (:require [net.cgrand.enlive-html :as html]
            [somnium.congomongo :as m])
  (:use [net.cgrand.moustache :only [app delegate]]
        [somnium.congomongo.config :only [*mongo-config*]]
        [ring.middleware.params :only [wrap-params]]
        [ring.middleware.keyword-params :only [wrap-keyword-params]]
;        [ring.middleware.reload :only [wrap-reload]]
        [ring.middleware.session :only [wrap-session]]
        [ring.middleware.file :only [wrap-file]]
        [ring.middleware.stacktrace :only [wrap-stacktrace]]
        [ring.util.response :only [response file-response redirect]]
        [ring.adapter.jetty :only [run-jetty]]))


;; middleware
(defn wrap-always
  "execute at every requestalways (first fns) : used for init DB connection"
  [handler & fns]
    (fn[req]
      (if (seq fns) ((first fns)))
      (handler req)))

;;;; mongo persistence

;;why (test (var notebook.web/split-mongo-url)) => :no-test ?
;;how to get :test ?
(defn split-mongo-url [url]
  "Parses mongodb url from heroku, eg. mongodb://user:pass@localhost:1234/db
or standard mongodb://localhost:27017/test
"
  {:test
     #(do
       (assert (= {:db "test", :port "27017", :host "localhost", :type "mongodb"}
              (split-mongo-url "mongodb://localhost:27017/test")))
       (assert (= {:db "db", :port "1234", :host "localhost", :pass "pass"
                   , :user "user", :type "mongodb"}
              (split-mongo-url "mongodb://user:pass@localhost:1234/db"))))
   }
  (let [infos (clojure.string/split url #"[/:@]+")]
    (condp = (count infos)
      6 (zipmap [:type :user :pass :host :port :db] infos)
      4 (zipmap [:type :host :port :db] infos)
      nil)))

(defn db-init []
  "Checks if connection, otherwise initialize."
  (when (not (m/connection? *mongo-config*)) ;; If global connection doesn't exist yet.
    (let [mongo-url (get (System/getenv)
                         "MONGOHQ_URL" "mongodb://localhost:27017/test") ;; Heroku puts it here.
          config (split-mongo-url mongo-url)] ;; Extract options.
      (println "Initializing mongo @ " mongo-url)
      (m/mongo! :db (:db config) :host (:host config)
                :port (Integer. (:port config))) ;; Setup global mongo.
      (when (:user config)
        (m/authenticate (:user config) (:pass config)))))) ;; Setup u/p.

(defn refresh-tag-counters []
  (let [tags (frequencies (mapcat :tags (m/fetch :notes)))]
    (do (m/destroy! :tags {})
        (m/mass-insert! :tags (map (fn[[t c]] (hash-map :_id t :total c)) tags))
      )))

(defn update-tag-counters [oldtags newtags]
  (doseq [tag (seq oldtags)]
      (m/fetch-and-modify :tags {:_id tag} {:$inc {:total -1}}))
  (doseq [tag (seq newtags)]
    (m/fetch-and-modify :tags {:_id tag} {:$inc {:total 1}} :upsert? true)))

(defn save [collection item]
  ;;update tag counters for note
  (if (= :notes collection)
    (update-tag-counters (:tags (m/fetch-by-id :notes (:_id item))) (:tags item)))
  (m/fetch-and-modify collection
                            {:_id (:_id item)}
                            (dissoc item :_id)
                            :upsert? true
                            :return-new? true))

(defn fetch
  "Fetch mongodb collection"
  [collection & {:keys [id tags]}]
  (if id
    (m/fetch-one collection :where {:_id (m/object-id id)})
    (m/fetch collection :where (if (seq tags) { :tags {:$in tags}} nil))))

;; HTML View Functions :


(defn render [t]
  (apply str t))

(def render-to-response
  (comp response render))

;; from https://github.com/swannodette/enlive-tutorial
(defn render-request [afn & args]
  (fn [req] (render-to-response (apply afn args))))


(defn prepend-attrs [att prefix]
  (fn[node] (update-in node [:attrs att] (fn[v] (str prefix v)))))

(defn coll->str [coll]
  (if (string? coll) coll (clojure.string/join " , " coll)))

(defn str->coll [s]
  (if (string? s) (clojure.string/split s #"[\W]+") s))


(html/defsnippet note-view "notebook.html" [:#note ]
  [{:keys [text tags _id] }]
  ;;insert into input text values if existing
  [[:p (html/nth-of-type 1)]] (html/content text)
  [[:p (html/nth-of-type 2)]] (html/content (coll->str tags))
  [:a.btn] (html/set-attr :href (str "/note/" _id)))


(html/defsnippet note-form "notebook.html" [:#enote]
  [{:keys [_id text tags]}]
  ;;insert into input text values if existing
  [[:input (html/attr= :name "_id")]] (html/set-attr :value _id)
  [:textarea] (html/content text)
  [[:input (html/attr= :name "tags")]] (html/set-attr :value (coll->str tags)))

(html/defsnippet login-form "notebook.html" [:#login] [])

(html/defsnippet menu-item "notebook.html" [:ul#menu [:li html/first-child]]
  [{:keys [active url title]}]
  [html/root] (if active (html/add-class "active") (html/remove-class "active"))
  [:a] (html/do->  (html/set-attr :href url)
                   (html/content title)))

(html/defsnippet status "notebook.html" [:#status]
  [user]
  [:a] (if user
         (html/substitute (str "Logged in as " user))
           (html/do-> (html/set-attr :href "/login")
                      (html/content "Login"))))

(html/deftemplate main "notebook.html" [{:keys [content menu status]}]
  [[:link (html/attr= :rel "stylesheet")]] (prepend-attrs :href "/")
  [:#status] (html/content status)
  [:#menu] (html/content menu)
  [:#content] (html/content content))

(defn build-tag-link [tag]
  {:url (str "/notes/" (:_id tag))
   :title (str (:_id tag) " (" (:total tag) ")")})

;;TODO set active if uri match a tag
(defn build-menu [uri]
  (map menu-item
       (concat
        (map build-tag-link
         (m/fetch :tags :sort {:_id 1}))
        (list {:url "/note" :title "Create Note"}))))

;; Add session key to initiate a session
;; TODO implement custom session storage (mongo) instead of in mem
;;
(defn authentificate[{params :params :as req}]
  (if-let [user (:user params)]
    (assoc (redirect "/notes")
      :session {:current-user user})
    ))

(defn logged[req]
  (-> req :session :current-user))

;; how to handle if view not found
(def views {:login (fn [& _] (login-form))
            :edit note-form
            :list (fn [notes] (map note-view (if (seq? notes) notes [notes])))})

(defn render-view
  ([type] (render-view type nil))
  ([type data]
     (fn [req] (render-to-response
                (main {:content ((views type) data)
                       :menu (build-menu (:uri req))
                       :status (status (logged req))})))))

(defn save-note
  "Only save note when use logged in"
  [{params :params :as req}]
  (if (logged req)
    (let [note (save :notes (update-in params [:tags] (fnil str->coll"")))]
      (render-view :list note))
    (redirect "/login")))



;; Description of the application
(def routes
  (app
;   (wrap-reload '[notebook.web])
   (wrap-file "resources")
   (wrap-stacktrace)
   (wrap-always db-init)
   (wrap-session)
   (wrap-params)
   (wrap-keyword-params)
   ["login"] {:get (render-view :login) :post authentificate}
   ["note"] {:get (render-view :edit) :post save-note}
   ["notes" & tags] (render-view :list (fetch :notes :tags tags))
   ["note" id] {:get (render-view :edit (fetch :notes :id id)) :post save-note}
   [&] (constantly (redirect "/notes"))
   ))


(defn start [ & [port & options]]
  (run-jetty (var routes) {:port (or port 8080) :join? false}))

(comment
  (w/routes {:uri "/note" :request-method :post :params {:text "test text" :tag ["misc"]}})
 )