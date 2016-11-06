(ns rete-tutorial
  (:require
   [pettomato.rete :refer [has-matches? trigger-next add-wme remove-wme add-until-stable]]
   [pettomato.rete.rete-macros :refer [compile-rules]]))

;;; A simple example.

;; Create the rete from rules. (Only one rule here.)
(def R (compile-rules {:preconds [[:a] [:b]]
                       :achieves [[:c]]
                       :deletes  [[:d]]}))

;; No matches because no facts have been added to the system.
(has-matches? R)

;; Pass it some facts.
(def R' (-> R
            (add-wme [:a])
            (add-wme [:b])))

;; Now there are matches.
(has-matches? R')

;; Trigger the next rule that is ready to fire (i.e., has matches).
(let [[R'' out] (trigger-next R')]
     out)

;; The facts in the output are wrapped with a polarity tag indicating
;; whether they were added, :+, or removed :-.


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


;;; Facts are more interesting when they have arguments.

;; Variables are prefixed with a ?.

(let [R (compile-rules {:preconds [[:a ?x ?y] [:b ?z]]
                        :achieves [[:c ?x ?y ?z]]})
      [R' out] (add-until-stable R [[:+ [:a 1 2]] [:+ [:b 3]] [:+ [:b 4]]])]
  out)

;; Two facts are produced because the :a term is able to match with
;; two different :b terms.


;;; Pattern matching.

;; Note that when a variable, like ?x, is used, it will match across
;; all instances in the rule.

(let [R (compile-rules {:preconds [[:a ?x] [:b ?x]]
                        :achieves [[:c ?x]]})
      [R' out] (add-until-stable R [[:+ [:a 1]] [:+ [:b 1]] [:+ [:b 2]]])]
  out)

;; Note that [:b 2] didn't match because there is no [:a 2] fact.


;;; Inline expressions.

(let [R (compile-rules {:preconds [[:a ?a] [:b ?b]]
                        :achieves [[:c (+ ?a ?b)]]})
      [R' out] (add-until-stable R [[:+ [:a 1]] [:+ [:b 1]] [:+ [:b 2]]])]
  out)


;;; Truth maintenance.

;; If a fact is retracted, you may want to retract any facts that were
;; supported by that fact.

(let [R (compile-rules {:preconds  [[:a ?a] [:b ?b]]
                        :achieves  [[:c (* ?a ?b)]]
                        :inv-match true}) ;; Change to false and eval again.
      [R' out] (add-until-stable R [[:+ [:a 3]] [:+ [:a 4]] [:+ [:b 5]]])]
  (println "out: " out)
  ;; Now remove one of the matching elements.
  (let [[R'' out] (add-until-stable R' [[:- [:a 3]]])]
    (println "out: " out)))


;;; Collapsing matches.

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
  (println "out: " out))


;;; Caching (to speed up truth maintenance).

;; If cache? is true, then the rule will be memoized with a special
;; memoization function that only remembers a result one time. In
;; other words, the second time the production is called with the same
;; input, it'll return the cached value, but then immediately forget
;; it. This is an optimization to avoid performing the calculation
;; again when removing a match.

(let [R (compile-rules {:preconds  [[:a ?x]]
                        :achieves  [[:b (do (Thread/sleep 1000) ?x)]]
                        :inv-match true
                        :cache?    true}) ;; Change to false and eval again.
      [R' out] (time (add-until-stable R [[:+ [:a 3]]]))
      [R'' out'] (time (add-until-stable R [[:- [:a 3]]]))]
  (println "out: " out)
  (println "out': " out'))


;;; Longhand rules.

;; The examples above use a shorthand notation for simple
;; productions. For complete control, the production rule can be
;; supplied. This is needed when the set of output fact types isn't
;; static.
(let [R (compile-rules {:preconds  [[:a ?a] [:b ?b]]
                        :fn        (fn [matches]
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
  (println "out: " out)
  (let [[R'' out] (add-until-stable R' [[:- [:a 1]]])]
    (println "out: " out)))
