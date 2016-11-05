(ns rete-tutorial
  (:require
   [pettomato.rete :refer [has-matches? trigger-next add-wme remove-wme add-until-stable]]
   [pettomato.rete.rete-macros :refer [compile-rules]]))

;; Note that both input values use the same variable.
(def R (compile-rules {:preconds  [[:a ?v] [:b ?v]]
                       :achieves  [[:c ?v]]
                       :deletes   [[:d ?v]]}))

(has-matches? R)

(def R' (-> R
            (add-wme [:a 5])
            (add-wme [:b 5])))

(has-matches? R')

(-> (trigger-next R')
    second)

(let [R (compile-rules {:preconds  [[:a ?a] [:b ?b]]
                        :achieves  [[:c (* ?a ?b)]]
                        :inv-match true}) ;; Change to false and eval again.
      [R' out] (add-until-stable R [[:+ [:a 3]] [:+ [:a 4]] [:+ [:b 5]]])]
  (println "out: " out)
  ;; Now remove one of the matching elements.
  (let [[R'' out] (add-until-stable R' [[:- [:a 3]]])]
    (println "out: " out)))

;;; The examples above used a shorthand notation for simple
;;; productions. For total control, the production rule can be
;;; supplied.
(let [R (compile-rules {:preconds  [[:a ?a] [:b ?b]]
                        :fn        (fn [matches]
                                     (reduce (fn [acc [op match]]
                                               (conj acc
                                                     (let [[[_ a] [_ b]] match]
                                                       (case op
                                                         :+ [:+ [:c (* a b)]]
                                                         :- [:- [:c (* a b)]]))))
                                             []
                                             matches))})
      [R' out] (add-until-stable R [[:+ [:a 3]] [:+ [:b 4]]])]
  (println "out: " out)
  (let [[R'' out] (add-until-stable R' [[:- [:a 3]]])]
    (println "out: " out)))

;; If collapse-matches? is true, then any pending matches that have
;; the same value, but opposite polarity will cancel out. If
;; collapse-matches? is false, or not supplied, then both values will
;; trigger the rule.

(let [R (compile-rules {:preconds  [[:a ?a] [:b ?b]]
                        :achieves  [[:c (* ?a ?b)]]}
                       {:preconds  [[:a ?a] [:b ?b]]
                        :deletes   [[:c (* ?a ?b)]]}
                       {:preconds  [[:c ?c]]
                        :achieves  [[:d ?c]]
                        :inv-match true
                        :collapse-matches? false}) ;; Change to true and eval again.
      [R' out] (add-until-stable R [[:+ [:a 3]] [:+ [:b 5]]])]
  (println "out: " out))

;; Rules are fired in the order they are written, unless a priority
;; value is supplied, in which case, they will run in order of highest
;; priority value.

(let [R (compile-rules {:preconds  [[:a ?a]]
                        :achieves  [[:third ?a]]}
                       {:preconds  [[:a ?a]]
                        :achieves  [[:first ?a]]
                        :priority  5}
                       {:preconds  [[:a ?a]]
                        :achieves  [[:second ?a]]
                        :priority  1}
                       {:preconds  [[:a ?a]]
                        :achieves  [[:fourth ?a]]})
      [R' out] (add-until-stable R [[:+ [:a 3]]])]
  (println "out: " out))

;; If cache? is true, then the rule will be memoized with a special
;; memoization function that only remembers a result one time. In
;; other words, the second time the production is called with the same
;; input, it'll return the cached value, but then immediately forget
;; it. This is an optimization to avoid performing the calculation
;; again when removing a match.

;;; TODO: cache? example.
