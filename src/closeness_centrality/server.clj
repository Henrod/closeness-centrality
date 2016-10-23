(ns closeness-centrality.server
	(:require
		[compojure.route :as route]
		[ring.middleware.params :refer [wrap-params]]
		[ring.middleware.reload :as reload]
		[ring.util.response :as res])
	(:use 
		[ring.adapter.jetty :only [run-jetty]]
    		[compojure.core :only [defroutes GET POST]]
    		[closeness-centrality.core :only [build-graph rank fraudulent add-connection]]))

(def graph (atom {}))
(def frauds (atom #{}))

(defn handler 
	[{params :params session :session} cmd]
	(cond
		(= cmd :rank) 	(res/response (str (rank (fraudulent @graph @frauds) true)))
		(= cmd :add) 	(let [src (params "src")  dst (params "dst")]
					         	(swap! graph add-connection [src dst])
						(res/response "Done"))
		(= cmd :fraud) 	(let [fraud (params "fraud")]
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