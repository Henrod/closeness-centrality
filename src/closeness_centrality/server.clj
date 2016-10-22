(ns closeness-centrality.server
	(:require
		[compojure.route :as route]
		[ring.middleware.session :refer [wrap-session]]
		[ring.middleware.params :refer [wrap-params]]
		[ring.middleware.reload :as reload]
		[ring.util.response :as res])
	(:use 
		[ring.adapter.jetty :only [run-jetty]]
    		[compojure.core :only [defroutes GET POST]]
    		[closeness-centrality.core :only [build-graph rank fraudulent add-connection]]))

(defn handler 
	[{params :params session :session}]
	(cond
		(params "logout") (-> (res/response "Session deleted") (assoc :session nil))
		(params "add") (let [src (params "src") 
					      dst (params "dst")
					      graph (add-connection (:graph session {}) src dst)
				         	      session (assoc session :graph graph)]
						(-> (res/response (str (rank graph :as-decimal)))
						       (assoc :session session)))))

(defroutes app-routes
	(POST "/" request (handler request))
  	(route/not-found "Not Found"))

(def app
	(-> app-routes
		(reload/wrap-reload)
		(wrap-session {:cookie-attrs {:max-age 3600}})
		(wrap-params)))

(defn -main []
	(println "Running on port 3000")
	(run-jetty app {:port 3000}))