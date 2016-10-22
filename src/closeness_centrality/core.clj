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
	(reduce (fn [m k] (assoc m k (/ 1 (bfs graph k)))) {} (keys graph)))

(defn rank 
	([graph]
	(sort-by second > (seq  (closeness graph))))
	([graph as-dec]
	(sort-by second > (seq  (if as-dec (as-decimal (closeness graph)) (closeness graph))))))

(defn fraudulent 
	[graph source]
	(if (contains? graph source)
		(let [fk (fn [e v] (* v (- 1 (math/expt 1/2 e))))]
			(loop [visited #{source}, adj (graph source), e 1, ncloseness (assoc (closeness graph) source 0)]
				(if (empty? adj)
					ncloseness
					(common-recur 
						graph visited adj e 
						#(reduce (fn [m k] (update m k (partial fk e))) ncloseness adj)))))
		(closeness graph)))

(defn read-file
	[path]
	(let [txt (slurp path) to-int (fn [s] (Integer. (re-matches #"[0-9]+" s)))]
		(reduce conj [] (map to-int (str/split txt #"\r\n|\s")))))

(defn add-connection
	[graph [src dst]]
	(if (and src dst)
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
