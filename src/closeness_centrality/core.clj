(ns closeness-centrality.core
	(:require 
		[clojure.math.numeric-tower :as math]
		[clojure.java.io :as io]
		[clojure.string :as str]
		[clojure.set :as set]))

(def select-values (comp vals select-keys))

(defmacro search-recur  [graph visited adj e func]
	`(recur
		(apply conj ~visited ~adj)
		(->> (select-values ~graph ~adj) (apply set/union) (filter (complement ~visited)))
		(inc ~e)
		(~func)))

(defn bfs [graph source]
	(loop [visited #{source}, adj (graph source), e 1, dists 0]
		(if (empty? adj)
			dists
			(search-recur 
				graph 
				visited 
				adj 
				e 
				#(->> adj (filter (complement visited)) count (* e) (+ dists))))))

(defn closeness [graph]
	(let [ks (keys graph)]
		(zipmap ks (pmap #(/ 1 (bfs graph %)) ks))))

(defn- as-decimal 
	[m]
	(zipmap (keys m) (pmap double (vals m))))

(defn rank
	([mclose]
		(sort #(compare (second %2) (second %1)) mclose))
	([mclose as-dec]
		(rank (if as-dec (as-decimal mclose) mclose))))

(defn- F<k> [e v]
	(* v (- 1 (math/expt 1/2 e))))

(defn fraudulent 
	[graph frauds]
	{:pre [(map? graph) (set? frauds)]}
	  (reduce 
	    (fn [ncloseness source]
		  (if (contains? graph source)
		    (loop [visited #{source}, adj (graph source), e 1, 
		    	         ncloseness (assoc ncloseness source 0)]
		      (if (empty? adj)
			     ncloseness
				  (search-recur graph visited adj e 
				   #(reduce 
				   	    (fn [m k] (if (contains? visited k) m (update m k (partial F<k> e)))) 
				   	    ncloseness 
				   	    adj))))
		  ncloseness))
	  (closeness graph) frauds))

(defn read-file
	[path]
	(let [txt (slurp path) to-int (fn [s] (Integer. (re-matches #"[0-9]+" s)))]
		(reduce conj [] (pmap to-int (str/split txt #"\r\n|\s")))))

(defn add-connection
	[graph [src dst]]
	(if (not= src dst)
		(let [partial-add (fn [m src dst]
                                           (let [adjs (m src #{})] 
				         			            (assoc m src (conj adjs dst))))]
			(-> (partial-add graph src dst)
			        (partial-add dst src)))
		graph))

(defn build-graph 
	[path]
	(let [input (read-file path)]
		(reduce add-connection {} (partition 2 input))))