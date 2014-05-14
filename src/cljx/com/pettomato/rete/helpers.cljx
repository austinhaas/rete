(ns com.pettomato.rete.helpers
  (:require
   [com.pettomato.rete :refer [add-wme remove-wme get-matches clear-matches]]))

(defn add-until-stable
  ;; dfs
  ([R ops] (add-until-stable R ops 100))
  ([R ops max-iterations]
     (loop [R           R
            open        (seq ops)
            all-matches ()
            safety      0]
       (assert (< safety max-iterations) safety)
       (if (empty? open)
         [R (reverse all-matches)]
         (let [[op w]  (first open)
               R'      (case op
                         :+ (add-wme R w)
                         :- (remove-wme R w))
               matches (get-matches R')]
           (if (empty? matches)
             (recur R' (rest open) all-matches (inc safety))
             (let [ms (reduce (fn [l x] (reduce #(cons %2 %1) l x)) () matches)]
               (recur (clear-matches R')
                      (into (rest open) ms)
                      (concat ms all-matches)
                      (inc safety)))))))))
