(ns closeness-centrality.core-test
  (:require [clojure.test :refer :all]
            [closeness-centrality.core :refer :all]))

(def little-graph
	{
		1 #{2 3}, 
		2 #{1 4}, 
		3 #{1}, 
		4 #{2}
	})

(deftest test-bfs
	(testing "BFS for little inputs"
		(are [x y] (= x y)
			4 (bfs little-graph 1)
			4 (bfs little-graph 2)
			6 (bfs little-graph 3)
			6 (bfs little-graph 4))))

(deftest test-closeness
	(testing "Closeness for little inputs"
		(let [cmap (closeness little-graph)]
			(are [x y] (= x y)
				1/4 (cmap 1)
				1/4 (cmap 2)
				1/6 (cmap 3)
				1/6 (cmap 4)))))

(deftest test-rank
	(testing "Rank for little inputs"
		(let [cmap (closeness little-graph)
		         rank-seq (rank little-graph)]
			(and
				(is (apply >= (map second rank-seq)))
				(is (map (fn [k v] (= (little-graph k) v) (rank-seq little-graph)))))))
	(testing "Rank for edges.txt"
		(let [graph (build-graph "test/closeness_centrality/edges.txt")
		         cmap (closeness graph)
		         rank-seq (rank graph)]
			(and
				(is (apply >= (map second rank-seq)))
				(is (map (fn [k v] (= (little-graph k) v) (rank-seq little-graph))))))))

(deftest test-fraudulent
	(testing "Fraudulent for little inputs"
		(are [x y] (= x y)
			(fraudulent little-graph 1) {
				1 0, 
				2  1/8, 
				3  1/12, 
				4  1/8}
			(fraudulent little-graph 2) {
				1  1/8, 
				2 0, 
				3  1/8, 
				4  1/12} 
			(fraudulent little-graph 3){
				1  1/8, 
				2  3/16, 
				3 0, 
				4  7/48}
			(fraudulent little-graph 4){
				1  3/16, 
				2  1/8, 
				3  7/48, 
				4 0})))

(deftest test-as-decimal
	(testing "test for some simple inputs"
		(is (let [m {:1 1/2 :2 1/4 :3 1/8}]
			(= (as-decimal m) {:1 0.5 :2 0.25 :3 0.125})))
		(is (let [m {1 1/6 "k" 1/3}]
			(= (as-decimal m) {1 (double 1/6) "k"  (double 1/3)})))))

(deftest test-read-file
	(testing "read little_input.txt"
		(is (= [1 2 1 3 2 4]
			(read-file "test/closeness_centrality/little_input.txt")))))

(deftest test-build-file
	(testing "build little input"
		(is (= little-graph 
			(build-graph "test/closeness_centrality/little_input.txt")))))
