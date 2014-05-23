(ns com.pettomato.rete.feedback
  (:require
   [com.pettomato.rete :refer [add-wme remove-wme has-matches? trigger-next]]))

(defn apply-op [R [op w]]
  (case op
    :+ (add-wme R w)
    :- (remove-wme R w)))

(defn add-until-stable
  ([R ops] (add-until-stable R ops 100))
  ([R ops max-iterations]
     (loop [R      R
            ops    (seq ops)
            acc    (vec ops)
            safety 0]
       (assert (< safety max-iterations) (str "safety:" safety))
       (let [R' (reduce apply-op R ops)]
         (if (has-matches? R')
           (let [[R'' out] (trigger-next R')]
             (recur R'' out (into acc out) (inc safety)))
           [R' acc])))))
