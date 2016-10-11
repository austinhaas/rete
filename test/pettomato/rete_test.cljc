(ns pettomato.rete-test
  (:require
   [clojure.test :refer :all]
   [pettomato.rete :refer [has-matches? trigger-next add-wme remove-wme add-until-stable]]
   #?(:clj [pettomato.rete.rete-macros :refer [compile-rules]]))
  #?(:cljs
     (:require-macros
      [pettomato.rete.rete-macros :refer [compile-rules]])))

(deftest basic-tests
  (let [R  (compile-rules {:preconds  [[:a ?v] [:b ?v]]
                           :achieves  [[:c ?v]]
                           :deletes   []})
        R' (-> R (add-wme [:a 5]) (add-wme [:b 5]))]
    (is (false? (has-matches? R)))
    (is (true?  (has-matches? R')))
    (is (= (second (trigger-next R')) [[:+ [:c 5]]]))
    ;; A wme that doesn't match any rule should not cause the Rete to
    ;; change.
    (is (= (add-wme R [:does-not-match 'does-not-match]) R))))

(deftest advanced-tests
  (let [R (compile-rules {:preconds  [[:a ?a] [:b ?b]]
                          :achieves  [[:c (+ ?a ?b)]]
                          :inv-match true
                          :collapse-matches? true}
                         {:preconds  [[:a ?a] [:b ?b] [:c ?c]]
                          :achieves  [[:d (+ ?a ?b ?c)]]
                          :inv-match true}
                         {:preconds  [[:d ?v] [:e ?v]]
                          :achieves  [[:f ?v]]
                          :inv-match true})]
    (let [[R' ms] (add-until-stable R [[:+ [:a 0]] [:+ [:b 1]] [:+ [:e 2]]])]
      (is (= ms [[:+ [:c 1]] [:+ [:d 2]] [:+ [:f 2]]])))
    (let [[R' ms] (add-until-stable R [[:+ [:a 0]] [:+ [:b 1]] [:- [:a 0]]])]
      (is (= ms [])))))

(deftest cache-test
  (let [i (atom 0)
        R (compile-rules {:preconds          [[:a ?a]]
                          :achieves          [[:b (swap! i inc)]]
                          :collapse-matches? false
                          :inv-match         true
                          :cache?            true})
        [R' ms] (add-until-stable R [[:+ [:a 0]] [:- [:a 0]]])]
    (is (= ms [[:+ [:b 1]] [:- [:b 1]]]))))

(deftest priority-test
  (let [R (compile-rules {:preconds [[:a ?a]]
                          :achieves [[:x ?a]]
                          :priority 2}
                         {:preconds [[:a ?a] [:b ?b]]
                          :achieves [[:y (+ ?a ?b)]]
                          :priority 3}
                         {:preconds [[:b ?b]]
                          :achieves [[:z ?b]]
                          :priority 1})
        [R' ms] (add-until-stable R [[:+ [:a 1]] [:+ [:b 2]]])]
    (is (= ms [[:+ [:y 3]] [:+ [:x 1]] [:+ [:z 2]]]))))

;; Test wme removal.

;; Test invert match fns.

;; (run-tests)
