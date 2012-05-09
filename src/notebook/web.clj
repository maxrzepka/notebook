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


(defn save [collection item]
  (m/insert! collection item))

(defn fetch [collection & {:keys [id tags]}]
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

(defmacro mydeftemplate
  "Same as deftemplate but make resources url absolute ( prepend / )"
  [name source args & forms]
  `(html/deftemplate ~name ~source ~args
     [[:link (html/attr= :rel "stylesheet")]] (prepend-attrs :href "/")
     ~@forms))

(defn coll->str [coll]
  (if (string? coll) coll (clojure.string/join " , " coll)))

(defn str->coll [s]
  (if (string? s) (clojure.string/split s #"[\W]+") s))


;;one HMTL page containing HTML form ,
(html/defsnippet note-view "notebook.html" [:#note ]
  [{:keys [text tags _id] }]
  ;;insert into input text values if existing
  [[:p (html/nth-of-type 1)]] (html/content text)
  [[:p (html/nth-of-type 2)]] (html/content (coll->str tags))
  [:a.btn] (html/set-attr :href (str "./note/" _id)))


(html/defsnippet note-form "notebook.html" [:#enote]
  [{:keys [id text tags]}]
  ;;insert into input text values if existing
  [:textarea] (html/content text)
  [:input] (html/content (coll->str tags)))

(html/defsnippet login-form "notebook.html" [:#login] [])

(mydeftemplate login "notebook.html" []
  [:#content] (html/content (login-form)))

;; TODO how to populate nav list : append path into href for each entry of navigation bar
(mydeftemplate edit-view "notebook.html" [note]
  [:#content] (html/content (note-form note)))

;;
(mydeftemplate list-view "notebook.html" [notes]
  [:#content] (html/content (map note-view (if (seq? notes) notes [notes]))))

;;
;; Add session key to initiate a session
;; TODO implement custom session storage (mongo) instead of in mem
;;
(defn authentificate[{params :params :as req}]
  (if-let [user (:user params)]
    (assoc (redirect "/notes")
      :session {:current-user user})
    ))

(defn login?[ {session :session }]
  (and session (:current-user session)))

;;
;; TODO move to mapreduce job to count tags
;; First attempt
;;  m = function() { var e = this.tags || [] ;
;; if( e.forEach ){e.forEach(function(value){ emit(this.value,1);}} )};
;;  r = function(key, values) { var res = { key: 0};
;;           values.forEach(function(value){ res.key += value;});}
;;
(defn save-note
  "Only save note when use logged in"
  [{params :params :as req}]
  (if (login? req)
    (let [tags (str->coll (:tags params ""))
          params (assoc params :tags tags)]
      (doseq [tag tags]
        (m/fetch-and-modify :tags {:_id tag} {:$inc {:value 1}} :upsert? true))
      (render-to-response (list-view
                         (save :notes params))))
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
   ["login"] {:get (render-request login) :post authentificate}
   ["note"] {:get (render-request edit-view nil) :post save-note}
   ["notes" & tags] (render-request list-view (fetch :notes :tags tags))
   ["note" id] {:get (render-request edit-view (fetch :notes :id id)) :post save-note}
   [&] (constantly (redirect "/notes"))
   ))


(defn start [ & [port & options]]
  (run-jetty (var routes) {:port (or port 8080) :join? false}))

(comment
  (w/routes {:uri "/note" :request-method :post :params {:text "test text" :tag ["misc"]}})
 )