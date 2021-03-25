(ns developer-test.core-test
  (:require [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [developer-test.core :as graphs]))

(def g {:1 [:2 :3]
        :2 [:4]
        :3 [:4]
        :4 []})

(def wg
  "weighted graph"
  {:1 ['(:2 1) '(:3 2)]
   :2 ['(:4 4)]
   :3 ['(:4 2)]
   :4 []})

(def wiki-graph
  "Sample graph from wikipedia
  (https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm)"
  `{:1 [(:2 ,7)  (:3 ,9)  (:6 ,14)]
    :2 [(:3 ,10) (:4 ,15)]
    :3 [(:1 ,9)  (:2 ,10) (:4 ,11) (:6 ,2)]
    :4 [(:2 ,15) (:3 ,11) (:5 ,6)]
    :5 [(:4 ,6)  (:6 ,9)]
    :6 [(:1 ,14) (:3 ,2)  (:5 ,9)]})

(st/instrument `graphs/graph)
(st/instrument `graphs/make-graph)

(t/deftest maintests

  (t/testing "Lazy graph traversal works with unweighted graphs"
    (t/is (s/valid? ::graphs/graph g))

    (t/is (= (graphs/seq-graph-dfs g :1)
             (list :1 :3 :4 :2)))

    (t/is (= (graphs/seq-graph-bfs g :1)
             (list :1 :2 :3 :4))))

  (t/testing "Lazy graph traversal works with weighted graphs"
    (t/is (= (graphs/seq-graph-dfs wg :1)
             (list :1 :3 :4 :2)))

    (t/is (= (graphs/seq-graph-bfs wg :1)
             (list :1 :2 :3 :4)))

    (t/is (s/valid? ::graphs/graph wg)))

  (t/testing "Graph generation"
    (st/check `generate-random-graph))


  (t/testing "shortest path"
    (let [[target cost path] (graphs/shortest-path wiki-graph :1 :5)]
       (t/is (= target :5))
       (t/is (= cost 23))
       (t/is (= path [:1 :6 :5]))))

  (t/testing "distance properties"
    (t/is (= 29 (graphs/eccentricity wiki-graph :1)))
    (t/is (= 17 (graphs/radius wiki-graph)))
    (t/is (= 30 (graphs/diameter wiki-graph)))))
