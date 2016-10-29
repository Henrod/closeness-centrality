(ns closeness-centrality.server
 (:require [compojure.route :as route]
 	       [ring.middleware.reload :as reload]
           [ring.util.response :as res]
           [ring.middleware.params :refer [wrap-params]]
           [ring.adapter.jetty :refer [run-jetty]]
           [compojure.core :refer [defroutes GET POST]]
           [closeness-centrality.core :refer [build-graph rank fraudulent add-connection]]))

(def graph (atom {}))
(def frauds (atom #{}))

(defn handler 
 [{params :params session :session} cmd]
 (case cmd
    :rank    (-> (fraudulent @graph @frauds)
    	                    (rank true)
    	                    str
    	                    res/response)
    :add     (let [src (params "src")  dst (params "dst")]
                    (swap! graph add-connection [src dst])
                    (res/response "Done"))
    :fraud  (let [fraud (params "fraud")]
                    (swap! frauds conj fraud)
                    (res/response "Done"))))

(defroutes app-routes
 (GET "/" request (handler request :rank))
 (POST "/add" request (handler request :add))
 (POST "/fraud" request (handler request :fraud))
   (route/not-found "Not Found"))

(def app
 (-> app-routes
  (reload/wrap-reload)
  (wrap-params)))

(defn -main []
 (println "Running on port 3000")
 (run-jetty app {:port 3000}))