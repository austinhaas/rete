(ns com.pettomato.rete-test
  (:require
   [clojure.test :refer :all]
   [com.pettomato.rete :refer [add-wme remove-wme]]
   [com.pettomato.rete.builder :refer [parse-and-compile-rules]]
   [com.pettomato.rete.feedback :refer [add-until-stable]]
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

(let [R (parse-and-compile-rules [(action->rule {:label      p1
                                                 :preconditions [[:a ?v1] [:b ?v2]]
                                                 :deletes    []
                                                 :achieves   [[:c (+ ?v1 ?v2)] [:q (+ ?v1 ?v2)]]}
                                                false
                                                false)
                                  (action->rule {:label      p2
                                                 :preconditions [[:c ?v1]]
                                                 :deletes    []
                                                 :achieves   [[:d (+ ?v1 ?v1)]]}
                                                false
                                                false)
                                  (action->rule {:label      p3
                                                 :preconditions [[:d ?v1]]
                                                 :deletes    []
                                                 :achieves   [[:e (+ ?v1 ?v1)]]}
                                                false
                                                false)])]
  (second (add-until-stable R [[:+ [:a 1]] [:+ [:a 3]] [:+ [:b 2]]])))

(defaction foo
  {:preconditions [[:a ?v1]]
   :deletes    []
   :achieves   [[:b (gensym)]]
   :priority   10}

)

(let [R (parse-and-compile-rules [foo])]
  (second (add-until-stable R [[:+ [:a 1]] [:- [:a 1]]])))
