(ns com.pettomato.rete-test
  (:require
   [clojure.test :refer :all]
   [com.pettomato.rete :refer [add-wme remove-wme get-matches clear-matches]]
   [com.pettomato.rete.builder :refer [parse-and-compile-rules]]
   [com.pettomato.rete.helpers :refer [add-until-stable]]
   #+clj
   [com.pettomato.rete.helper-macros :refer [action->rule defaction]])
  #+cljs
  (:require-macros
   [com.pettomato.rete.helper-macros :refer [action->rule defaction]]))

(defn adder+ [ms x] (for [m ms] [:+ [x (apply + (map second m))]]))
(defn adder- [ms x] (for [m ms] [:- [x (apply + (map second m))]]))

(deftest basic-tests
  (let [R (parse-and-compile-rules 0 [{:conditions  '[[:a ?v] [:b ?v]]
                                       :add-matches #(adder+ % :c)
                                       :rem-matches #(adder- % :c)}
                                      {:conditions  '[[:d ?v1] [:e ?v2]]
                                       :add-matches #(adder+ % :f)
                                       :rem-matches #(adder- % :f)}
                                      {:conditions  '[[:c ?v1] [:f ?v2]]
                                       :add-matches #(adder+ % :g)
                                       :rem-matches #(adder- % :g)}
                                      {:conditions  '[[:a ?v1] [:b ?v2] [:d ?v3]]
                                       :add-matches #(adder+ % :h)
                                       :rem-matches #(adder- % :h)}])]
    (is (= (clear-matches R) R))
    (is (= (get-matches R) []))
    (is (= (add-wme R [:does-not-match 'does-not-match]) R))
    (is (= (get-matches (add-wme R [:a 0])) []))
    (is (= (get-matches (reduce add-wme R '[[:a 10] [:b 10]])) [[[:+ [:c 20]]]]))
    (is (= (get-matches (reduce add-wme R '[[:a  5] [:b 10]])) []))
    (is (= (get-matches (reduce add-wme R '[[:a 10] [:b 10] [:d 5] [:e 3]]))
           [[[:+ [:c 20]]] [[:+ [:h 25]]] [[:+ [:f 8]]]]))
    (is (= (get-matches (remove-wme (reduce add-wme R '[[:a 10] [:b 10] [:d 5] [:e 3]])
                                    [:a 10]))
           [[[:+ [:c 20]]] [[:+ [:h 25]]] [[:+ [:f 8]]] [[:- [:c 20]]] [[:- [:h 25]]]]))))



(deftest beta-sharing
  (let [R (parse-and-compile-rules 0
                                   [{:conditions  '[[:a ?x]]
                                     :add-matches identity
                                     :rem-matches identity}
                                    {:conditions  '[[:a ?y]]
                                     :add-matches identity
                                     :rem-matches identity}
                                    {:conditions  '[[:a ?x] [:c ?x]]
                                     :add-matches identity
                                     :rem-matches identity}])]
    (is (= (get-matches (reduce add-wme R '[[:a 5] [:c 5]]))
           [[[[:a 5]]] [[[:a 5]]] [[[:a 5] [:c 5]]]]))))

(deftest consistency-tests
  (let [R (parse-and-compile-rules 0
                                   [{:conditions '[[:a ?x] [:b ?y] (< ?x ?y)]
                                     :add-matches identity
                                     :rem-matches identity}])]
    (is (= (get-matches (reduce add-wme R '[[:a 1] [:b 2]]))
           [[[[:a 1] [:b 2]]]]))
    (is (= (get-matches (reduce add-wme R '[[:a 2] [:b 2]]))
           []))))

(deftest consistency-ops
  (let [R (parse-and-compile-rules 0
                                   [{:conditions '[[:a ?x] [:b ?y] (= (+ ?x ?x) ?y)]
                                     :add-matches identity
                                     :rem-matches identity}])]
    (is (= (get-matches (reduce add-wme R '[[:a 1] [:b 2]]))
           [[[[:a 1] [:b 2]]]]))
    (is (= (get-matches (reduce add-wme R '[[:a 2] [:b 2]]))
           []))))

(deftest actions
  (let [as [{:preconditions [[:a '?v]]
             :deletes       [[]]
             :achieves      [[:b '?v]]}
            {:preconditions [[:b '?v] (list < '?v 5)]
             :deletes       [[]]
             :achieves      [[:a (list inc '?v)]]}]
        rs (map #(action->rule % true) as)
        R  (parse-and-compile-rules 0 rs)]
    (second (add-until-stable R [[:+ [:a 0]] [:- [:a 0]]]))))

(run-tests)

(let [R (parse-and-compile-rules 0 [(action->rule {:preconditions [[:a ?v1] [:b ?v2]]
                                                   :deletes    []
                                                   :achieves   [[:c (+ ?v1 ?v2)]]}
                                                  false)
                                    (action->rule {:preconditions [[:c ?v1]]
                                                   :deletes    []
                                                   :achieves   [[:d (+ ?v1 ?v1)]]}
                                                  false)
                                    (action->rule {:preconditions [[:d ?v1]]
                                                   :deletes    []
                                                   :achieves   [[:e (+ ?v1 ?v1)]]}
                                                  false)])]
  (apply concat (get-matches (add-until-stable R [[:+ [:a 1]] [:+ [:b 2]]]))))

(count [[:- [:agent-1 :vel [0 0 0]]]
[:- [[1 2 0]
:available true]]
[:- [:agent-1 :moving-forward false]]
[:+ [:agent-1 :moving-forward true]]
[:+ [:agent-1 :action-start 200]]
[:+ [:agent-1 :action-finish 1000]]
[:+ [:agent-1 :target [1 2 0]]]
[:+ [:agent-1 :vel [0 1 0]]]
[:- [:agent-2 :rotating-right false]]
[:+ [:agent-2 :rotating-right true]]
[:+ [:agent-2 :action-start 200]]
[:+ [:agent-2 :action-finish 400]]
[:+ [:agent-2 :target [-1 0 0]]]
[:- [:agent-3 :rotating-right false]]
[:+ [:agent-3 :rotating-right true]]
[:+ [:agent-3 :action-start 200]]
[:+ [:agent-3 :action-finish 400]]
[:+ [:agent-3 :target [-1 0 0]]]
[:- [:agent-4 :rotating-left false]]
[:+ [:agent-4 :rotating-left true]]
[:+ [:agent-4 :action-start 200]]
[:+ [:agent-4 :action-finish 400]]
[:+ [:agent-4 :target [1 0 0]]]
[:- [:agent-5 :vel [0 0 0]]]
[:- [[5 6 0]
:available true]]
[:- [:agent-5 :moving-forward false]]
[:+ [:agent-5 :moving-forward true]]
[:+ [:agent-5 :action-start 200]]
[:+ [:agent-5 :action-finish 1000]]
[:+ [:agent-5 :target [5 6 0]]]
[:+ [:agent-5 :vel [0 1 0]]]
[:- [:agent-1 :rotating-right false]]
[:+ [:agent-1 :rotating-right true]]
[:+ [:agent-1 :action-start 200]]
[:+ [:agent-1 :action-finish 400]]
[:+ [:agent-1 :target [-1 0 0]]]
[:- [:agent-2 :vel [0 0 0]]]
[:- [[10 11 0]
:available true]]
[:- [:agent-2 :moving-forward false]]
[:+ [:agent-2 :moving-forward true]]
[:+ [:agent-2 :action-start 200]]
[:+ [:agent-2 :action-finish 1000]]
[:+ [:agent-2 :target [10 11 0]]]
[:+ [:agent-2 :vel [0 1 0]]]
[:- [:agent-3 :rotating-right false]]
[:+ [:agent-3 :rotating-right true]]
[:+ [:agent-3 :action-start 200]]
[:+ [:agent-3 :action-finish 400]]
[:+ [:agent-3 :target [-1 0 0]]]
[:- [:agent-4 :rotating-left false]]
[:+ [:agent-4 :rotating-left true]]
[:+ [:agent-4 :action-start 200]]
[:+ [:agent-4 :action-finish 400]]
[:+ [:agent-4 :target [1 0 0]]]
[:- [:agent-5 :rotating-left false]]
[:+ [:agent-5 :rotating-left true]]
[:+ [:agent-5 :action-start 200]]
[:+ [:agent-5 :action-finish 400]]
[:+ [:agent-5 :target [1 0 0]]]
[:- [:agent-2 :action-start 200]]
[:- [:agent-2 :action-finish 400]]
[:- [:agent-2 :target [-1 0 0]]]
[:- [:agent-2 :rot [0 1 0]]]
[:- [:agent-2 :rotating-right true]]
[:+ [:agent-2 :rot [-1 0 0]]]
[:+ [:agent-2 :rotating-right false]]
[:- [:agent-3 :action-start 200]]
[:- [:agent-3 :action-finish 400]]
[:- [:agent-3 :target [-1 0 0]]]
[:- [:agent-3 :rot [0 1 0]]]
[:- [:agent-3 :rotating-right true]]
[:+ [:agent-3 :rot [-1 0 0]]]
[:+ [:agent-3 :rotating-right false]]
[:- [:agent-4 :action-start 200]]
[:- [:agent-4 :action-finish 400]]
[:- [:agent-4 :target [1 0 0]]]
[:- [:agent-4 :rot [0 1 0]]]
[:- [:agent-4 :rotating-left true]]
[:+ [:agent-4 :rot [1 0 0]]]
[:+ [:agent-4 :rotating-left false]]
[:- [:agent-1 :action-start 200]]
[:- [:agent-1 :action-finish 400]]
[:- [:agent-1 :target [1 2 0]]]
[:- [:agent-1 :rot [0 1 0]]]
[:- [:agent-1 :rotating-right true]]
[:+ [:agent-1 :rot [1 2 0]]]
[:+ [:agent-1 :rotating-right false]]
[:- [:agent-1 :action-start 200]]
[:- [:agent-1 :action-finish 400]]
[:- [:agent-1 :target [1 2 0]]]
[:- [:agent-1 :vel [0 1 0]]]
[:- [:agent-1 :pos [1 1 0]]]
[:- [:agent-1 :moving-forward true]]
[:+ [:agent-1 :vel [0 0 0]]]
[:+ [:agent-1 :pos [1 2 0]]]
[:+ [[1 1 0]
:available true]]
[:+ [:agent-1 :moving-forward false]]
[:- [:agent-1 :action-start 200]]
[:- [:agent-1 :action-finish 400]]
[:- [:agent-1 :target [-1 0 0]]]
[:- [:agent-1 :rot [0 1 0]]]
[:- [:agent-1 :rotating-right true]]
[:+ [:agent-1 :rot [-1 0 0]]]
[:+ [:agent-1 :rotating-right false]]
[:- [:agent-1 :action-start 200]]
[:- [:agent-1 :action-finish 400]]
[:- [:agent-1 :target [-1 0 0]]]
[:- [:agent-1 :vel [0 1 0]]]
[:- [:agent-1 :pos [1 1 0]]]
[:- [:agent-1 :moving-forward true]]
[:+ [:agent-1 :vel [0 0 0]]]
[:+ [:agent-1 :pos [-1 0 0]]]
[:+ [[1 1 0]
:available true]]
[:+ [:agent-1 :moving-forward false]]
[:- [:agent-2 :action-start 200]]
[:- [:agent-2 :action-finish 400]]
[:- [:agent-2 :target [-1 0 0]]]
[:- [:agent-2 :rot [0 1 0]]]
[:- [:agent-2 :rotating-right true]]
[:+ [:agent-2 :rot [-1 0 0]]]
[:+ [:agent-2 :rotating-right false]]
[:- [:agent-2 :action-start 200]]
[:- [:agent-2 :action-finish 400]]
[:- [:agent-2 :target [10 11 0]]]
[:- [:agent-2 :rot [0 1 0]]]
[:- [:agent-2 :rotating-right true]]
[:+ [:agent-2 :rot [10 11 0]]]
[:+ [:agent-2 :rotating-right false]]
[:- [:agent-2 :action-start 200]]
[:- [:agent-2 :action-finish 400]]
[:- [:agent-2 :target [-1 0 0]]]
[:- [:agent-2 :vel [0 1 0]]]
[:- [:agent-2 :pos [10 10 0]]]
[:- [:agent-2 :moving-forward true]]
[:+ [:agent-2 :vel [0 0 0]]]
[:+ [:agent-2 :pos [-1 0 0]]]
[:+ [[10 10 0]
:available true]]
[:+ [:agent-2 :moving-forward false]]
[:- [:agent-2 :action-start 200]]
[:- [:agent-2 :action-finish 400]]
[:- [:agent-2 :target [10 11 0]]]
[:- [:agent-2 :vel [0 1 0]]]
[:- [:agent-2 :pos [10 10 0]]]
[:- [:agent-2 :moving-forward true]]
[:+ [:agent-2 :vel [0 0 0]]]
[:+ [:agent-2 :pos [10 11 0]]]
[:+ [[10 10 0]
:available true]]
[:+ [:agent-2 :moving-forward false]]
[:- [:agent-3 :action-start 200]]
[:- [:agent-3 :action-finish 400]]
[:- [:agent-3 :target [-1 0 0]]]
[:- [:agent-3 :rot [0 1 0]]]
[:- [:agent-3 :rotating-right true]]
[:+ [:agent-3 :rot [-1 0 0]]]
[:+ [:agent-3 :rotating-right false]]
[:- [:agent-3 :action-start 200]]
[:- [:agent-3 :action-finish 400]]
[:- [:agent-3 :target [-1 0 0]]]
[:- [:agent-3 :rot [0 1 0]]]
[:- [:agent-3 :rotating-right true]]
[:+ [:agent-3 :rot [-1 0 0]]]
[:+ [:agent-3 :rotating-right false]]
[:- [:agent-3 :action-start 200]]
[:- [:agent-3 :action-finish 400]]
[:- [:agent-3 :target [-1 0 0]]]
[:- [:agent-3 :rot [0 1 0]]]
[:- [:agent-3 :rotating-right true]]
[:+ [:agent-3 :rot [-1 0 0]]]
[:+ [:agent-3 :rotating-right false]]
[:- [:agent-3 :action-start 200]]
[:- [:agent-3 :action-finish 400]]
[:- [:agent-3 :target [-1 0 0]]]
[:- [:agent-3 :rot [0 1 0]]]
[:- [:agent-3 :rotating-right true]]
[:+ [:agent-3 :rot [-1 0 0]]]
[:+ [:agent-3 :rotating-right false]]
[:- [:agent-4 :action-start 200]]
[:- [:agent-4 :action-finish 400]]
[:- [:agent-4 :target [1 0 0]]]
[:- [:agent-4 :rot [0 1 0]]]
[:- [:agent-4 :rotating-left true]]
[:+ [:agent-4 :rot [1 0 0]]]
[:+ [:agent-4 :rotating-left false]]
[:- [:agent-4 :action-start 200]]
[:- [:agent-4 :action-finish 400]]
[:- [:agent-4 :target [1 0 0]]]
[:- [:agent-4 :rot [0 1 0]]]
[:- [:agent-4 :rotating-left true]]
[:+ [:agent-4 :rot [1 0 0]]]
[:+ [:agent-4 :rotating-left false]]
[:- [:agent-4 :action-start 200]]
[:- [:agent-4 :action-finish 400]]
[:- [:agent-4 :target [1 0 0]]]
[:- [:agent-4 :rot [0 1 0]]]
[:- [:agent-4 :rotating-left true]]
[:+ [:agent-4 :rot [1 0 0]]]
[:+ [:agent-4 :rotating-left false]]
[:- [:agent-4 :action-start 200]]
[:- [:agent-4 :action-finish 400]]
[:- [:agent-4 :target [1 0 0]]]
[:- [:agent-4 :rot [0 1 0]]]
[:- [:agent-4 :rotating-left true]]
[:+ [:agent-4 :rot [1 0 0]]]
[:+ [:agent-4 :rotating-left false]]
[:- [:agent-5 :action-start 200]]
[:- [:agent-5 :action-finish 400]]
[:- [:agent-5 :target [5 6 0]]]
[:- [:agent-5 :rot [0 1 0]]]
[:- [:agent-5 :rotating-left true]]
[:+ [:agent-5 :rot [5 6 0]]]
[:+ [:agent-5 :rotating-left false]]
[:- [:agent-5 :action-start 200]]
[:- [:agent-5 :action-finish 400]]
[:- [:agent-5 :target [5 6 0]]]
[:- [:agent-5 :vel [0 1 0]]]
[:- [:agent-5 :pos [5 5 0]]]
[:- [:agent-5 :moving-forward true]]
[:+ [:agent-5 :vel [0 0 0]]]
[:+ [:agent-5 :pos [5 6 0]]]
[:+ [[5 5 0]
:available true]]
[:+ [:agent-5 :moving-forward false]]
[:- [:agent-5 :action-start 200]]
[:- [:agent-5 :action-finish 400]]
[:- [:agent-5 :target [1 0 0]]]
[:- [:agent-5 :rot [0 1 0]]]
[:- [:agent-5 :rotating-left true]]
[:+ [:agent-5 :rot [1 0 0]]]
[:+ [:agent-5 :rotating-left false]]
[:- [:agent-5 :action-start 200]]
[:- [:agent-5 :action-finish 400]]
[:- [:agent-5 :target [1 0 0]]]
[:- [:agent-5 :vel [0 1 0]]]
[:- [:agent-5 :pos [5 5 0]]]
[:- [:agent-5 :moving-forward true]]
[:+ [:agent-5 :vel [0 0 0]]]
[:+ [:agent-5 :pos [1 0 0]]]
[:+ [[5 5 0]
:available true]]
[:+ [:agent-5 :moving-forward false]]])


