(ns com.pettomato.rete-test
  (:require
   [clojure.test :refer :all]
   [com.pettomato.rete.builder :refer [parse-and-compile-rules]]
   [com.pettomato.rete :refer [add-wme remove-wme get-matches clear-matches]]))

(defn adder+ [ms x] (for [m ms] ['+ [x (apply + (map second m))]]))
(defn adder- [ms x] (for [m ms] ['- [x (apply + (map second m))]]))

(def test-rules
  [{:conditions  '[[:a ?v] [:b ?v]]
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
    :rem-matches #(adder- % :h)}])

(deftest basic-tests
  (let [R (parse-and-compile-rules 0 test-rules)]
    (is (= (clear-matches R) R))
    (is (= (get-matches R) []))
    (is (= (add-wme R [:does-not-match 'does-not-match]) R))
    (is (= (get-matches (add-wme R [:a 0])) []))
    (is (= (get-matches (reduce add-wme R '[[:a 10] [:b 10]])) [[['+ [:c 20]]]]))
    (is (= (get-matches (reduce add-wme R '[[:a  5] [:b 10]])) []))
    (is (= (get-matches (reduce add-wme R '[[:a 10] [:b 10] [:d 5] [:e 3]]))
           [[['+ [:c 20]]] [['+ [:h 25]]] [['+ [:f 8]]]]))))
