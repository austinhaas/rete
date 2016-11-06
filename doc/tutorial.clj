(ns rete-tutorial
  (:require
   [pettomato.rete :refer [has-matches? trigger-next add-wme remove-wme add-until-stable]]
   [pettomato.rete.rete-macros :refer [compile-rules]]))

;;; A simple example.

;; Create the rete from rules. (Only one rule here.)
(def R (compile-rules {:preconds [[:a] [:b]]
                       :achieves [[:c]]
                       :deletes  [[:d]]}))


(has-matches? R) ;; => false

;; No matches because no facts have been added to the system.

;; Pass it some facts.
(def R' (-> R
            (add-wme [:a])
            (add-wme [:b])))

(has-matches? R') ;; => true

;; Trigger the next rule that is ready to fire (i.e., has matches).
(let [[R'' out] (trigger-next R')]
  out)

;; => [[:- [:d]] [:+ [:c]]]

;; The facts in the output are wrapped with a polarity tag indicating
;; whether they were added, :+, or removed, :-.


;;; Multiple rules.

;; Usually, we'll want to take facts that are output from a rule and
;; pass them back into the system to trigger other rules. This cycle
;; continues until the system stabilizes. The add-until-stable
;; function performs this behavior.

(let [R (compile-rules {:preconds [[:a] [:b]]
                        :achieves [[:c]]}
                       {:preconds [[:c]]
                        :achieves [[:d]]}
                       {:preconds [[:a] [:b] [:d]]
                        :achieves [[:e]]})
      [R' out] (add-until-stable R [[:+ [:a]] [:+ [:b]]])]
  out)

;; => [[:+ [:c]] [:+ [:d]] [:+ [:e]]]


;;; Facts are more interesting when they have arguments.

;; Variables are prefixed with a ?.

(let [R (compile-rules {:preconds [[:a ?x ?y] [:b ?z]]
                        :achieves [[:c ?x ?y ?z]]})
      [R' out] (add-until-stable R [[:+ [:a 1 2]] [:+ [:b 3]] [:+ [:b 4]]])]
  out)

;; => [[:+ [:c 1 2 3]] [:+ [:c 1 2 4]]]

;; Two facts are produced because the :a term is able to match with
;; two different :b terms.


;;; Pattern matching.

;; Note that when a variable, like ?x, is used, it will match across
;; all instances in the rule.

(let [R (compile-rules {:preconds [[:a ?x] [:b ?x]]
                        :achieves [[:c ?x]]})
      [R' out] (add-until-stable R [[:+ [:a 1]] [:+ [:b 1]] [:+ [:b 2]]])]
  out)

;; => [[:+ [:c 1]]]

;; Note that [:b 2] didn't match because there is no [:a 2] fact.


;;; Inline expressions.

(let [R (compile-rules {:preconds [[:a ?a] [:b ?b]]
                        :achieves [[:c (+ ?a ?b)]]})
      [R' out] (add-until-stable R [[:+ [:a 1]] [:+ [:b 2]] [:+ [:b 3]]])]
  out)

;; => [[:+ [:c 3]] [:+ [:c 4]]]


;;; Truth maintenance.

;; If a fact is retracted, you may want to retract any facts that were
;; supported by that fact.

(let [R (compile-rules {:preconds  [[:a ?a] [:b ?b]]
                        :achieves  [[:c (* ?a ?b)]]
                        :inv-match true}) ;; Change to false and eval again.
      [R' out] (add-until-stable R [[:+ [:a 3]] [:+ [:a 4]] [:+ [:b 5]]])]
  (println out)
  ;; Now remove one of the matching elements.
  (let [[R'' out] (add-until-stable R' [[:- [:a 3]]])]
    (println out)))

;; => [[:+ [:c 20]] [:+ [:c 15]]]
;; => [[:- [:c 15]]]

;;; Collapsing matches.

;; If collapse-matches? is true, then any pending matches that have
;; the same value, but opposite polarity will cancel out. If
;; collapse-matches? is false, or not supplied, then both values will
;; trigger the rule.

;; Here we have two rules that produce values that should cancel
;; out. The third rule depends on these values. The default behavior
;; is to pass everything through without modification. By changing
;; collapse-matches? to true, we can make these values cancel out
;; before the production is triggered.

(let [R (compile-rules {:preconds  [[:a ?a] [:b ?b]]
                        :achieves  [[:c (* ?a ?b)]]}
                       {:preconds  [[:a ?a] [:b ?b]]
                        :deletes   [[:c (* ?a ?b)]]}
                       {:preconds  [[:c ?c]]
                        :achieves  [[:d ?c]]
                        :inv-match true
                        :collapse-matches? false}) ;; Change to true and eval again.
      [R' out] (add-until-stable R [[:+ [:a 3]] [:+ [:b 5]]])]
  out)

;; => [[:+ [:c 15]] [:- [:c 15]] [:+ [:d 15]] [:- [:d 15]]]

;; After changing :collapse-matches? to true:

;; => [[:+ [:c 15]] [:- [:c 15]]]


;;; Rule priority.

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
  out)

;; => [[:+ [:first 3]] [:+ [:second 3]] [:+ [:third 3]] [:+ [:fourth 3]]]


;;; Caching (to speed up truth maintenance).

;; If cache? is true, then the rule will be memoized with a special
;; memoization function that only remembers a result once. In other
;; words, the second time the production is called with the same
;; input, it'll return the cached value, and then immediately forget
;; it. This is an optimization to avoid performing the calculation
;; again when removing a match.

(let [R (compile-rules {:preconds  [[:a ?x]]
                        :achieves  [[:b (do (Thread/sleep 1000) ?x)]]
                        :inv-match true
                        :cache?    true}) ;; Change to false and eval again.
      [R' out] (time (add-until-stable R [[:+ [:a 3]]]))
      [R'' out'] (time (add-until-stable R [[:- [:a 3]]]))]
  (println out)
  (println out'))

;; => "Elapsed time: 1002.107053 msecs"
;; => "Elapsed time: 2.902768 msecs"
;; => [[:+ [:b 3]]]
;; => [[:- [:b 3]]]


;;; Longhand rules.

;; The examples above use a shorthand notation for simple
;; productions. For complete control, the production rule can be
;; supplied. This is needed when the set of output fact types isn't
;; static.
(let [R (compile-rules {:preconds [[:a ?a] [:b ?b]]
                        :fn       (fn [matches]
                                    (reduce (fn [acc [op match]]
                                              (conj acc
                                                    (let [[[_ a] [_ b]] match
                                                          result (+ a b)
                                                          type   (if (odd? result)
                                                                   :odd
                                                                   :even)]
                                                      (case op
                                                        :+ [:+ [type result]]
                                                        :- [:- [type result]]))))
                                            []
                                            matches))})
      [R' out] (add-until-stable R [[:+ [:a 1]] [:+ [:b 2]] [:+ [:a 2]]])]
  (println out)
  (let [[R'' out] (add-until-stable R' [[:- [:a 1]]])]
    (println out)))

;; => [[:+ [:odd 3]] [:+ [:even 4]]]
;; => [[:- [:odd 3]]]
