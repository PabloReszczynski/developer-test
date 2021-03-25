(ns developer-test.core-test
  (:require [clojure.test :as t]
            [developer-test.core :as graphs]))

(def g {:1 [:2 :3]
        :2 [:4]
        :3 [:4]
        :4 []})

(t/is (= (graphs/seq-graph-dfs g :1)
         (list :1 :3 :4 :2)))

(t/is (= (graphs/seq-graph-bfs g :1)
         (list :1 :2 :3 :4)))

(def weighted-g
  {:1 ['(:2 1) '(:3 2)]
   :2 ['(:4 4)]
   :3 ['(:4 2)]
   :4 []})
