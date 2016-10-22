(defproject closeness-centrality "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
                   :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main closeness-centrality.server
  :dependencies [[org.clojure/clojure "1.8.0"]
  			 [org.clojure/math.numeric-tower "0.0.4"]
  			 [compojure "1.4.0"]
  			 [ring "1.5.0"] ]
  :plugins [[lein-ring "0.9.7"]]
  :profiles
  	{:dev {:dependencies [[ring/ring-mock "0.3.0"]
                        			 [ring/ring-devel "1.1.8"]]}})
