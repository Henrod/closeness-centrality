(ns closeness-centrality.core
	(:require 
		[clojure.math.numeric-tower :as math]
		[clojure.java.io :as io]
		[clojure.string :as str]
		[clojure.set :as set]))

(defn as-decimal 
	[m]
	(zipmap (keys m) (map double (vals m))))

(defmacro common-recur 
	[graph visited adj e func]
	`(recur
		(apply conj ~visited ~adj)
		(distinct (filter (complement ~visited) (apply set/union (map #(~graph %) ~adj))))
		(inc ~e)
		(~func)))

(defn bfs 
	[graph source]
	(loop [visited #{source}, adj (graph source), e 1, dists 0]
		(if (empty? adj)
			dists
			(common-recur 
				graph visited adj e 
				#(+ dists (* e (count (filter (complement visited) adj))))))))

(defn closeness
	[graph]
	(let [ks (keys graph)]
		(zipmap ks (pmap #(->> % (bfs graph) (/ 1)) ks))))

(defn rank
	([mclose]
		(into (sorted-map-by 
			(fn [k1 k2] 
				(<= (mclose k2) (mclose k1))))
			mclose))
	([mclose as-dec]
		(into (sorted-map-by 
			(fn [k1 k2] 
				(<= (mclose k2) (mclose k1))))
			(if as-dec (as-decimal mclose) mclose))))

(defn fraudulent 
	[graph frauds]
	(let [fk (fn [e v] (* v (- 1 (math/expt 1/2 e))))]
		(reduce 
			(fn [ncloseness source]
				(if (and (contains? graph source) (pos? (ncloseness source)))
					(loop [visited #{source}, adj (graph source), e 1, ncloseness (assoc ncloseness source 0)]
						(if (empty? adj)
							ncloseness
							(common-recur 
								graph visited adj e 
								#(reduce (fn [m k] (update m k (partial fk e))) ncloseness adj))))
					ncloseness))
			(closeness graph) frauds)))

(defn read-file
	[path]
	(let [txt (slurp path) to-int (fn [s] (Integer. (re-matches #"[0-9]+" s)))]
		(reduce conj [] (map to-int (str/split txt #"\r\n|\s")))))

(defn add-connection
	[graph [src dst]]
	(if (and src dst (not= src dst))
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
