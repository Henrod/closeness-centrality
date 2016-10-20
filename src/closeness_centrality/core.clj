(ns closeness-centrality.core
	(:require [clojure.math.numeric-tower :as math]))

(def little-graph
	{1  [2 3]
	  2 [1 4]
	  3 [1]
	  4 [2]})

(defn bfs [graph source]
	(loop [visited #{source}, adj (graph source), e 1, dists 0]
		(if (empty? adj)
			dists
			(recur
				(apply conj visited adj)
				(distinct (filter (complement visited) (apply concat (map #(graph %) adj))))
				(inc e)
				(+ dists (* e (count (filter (complement visited) adj))))))))

(defn closeness [graph]
	(reduce (fn [m k] (assoc m k (/ 1 (bfs graph k)))) {} (keys graph)))

(defn rank [graph]
	(sort-by second < (seq (closeness graph))))

(defn fraudulent [graph source]
	(if (contains? graph source)
		(let [fk (fn [e v] (* v (- 1 (math/expt 1/2 e))))]
			(loop [visited #{source}, adj (graph source), e 1, ncloseness (assoc (closeness graph) source 0)]
				(if (empty? adj)
					ncloseness
					(recur
						(apply conj visited adj)
						(distinct (filter (complement visited) (apply concat (map #(graph %) adj))))
						(inc e)
						(reduce (fn [m k] (update m k (partial fk e))) ncloseness adj)))))
		(closeness graph)))

(defn as-decimal [m]
	(zipmap (keys m) (map double (vals m))))