(ns com.pettomato.rete.helpers
  (:require
   [com.pettomato.rete :refer [add-wme remove-wme get-matches clear-matches]]))

(defn apply-op [R [op w]]
  (case op
    :+ (add-wme R w)
    :- (remove-wme R w)))

(defn consume-matches [R]
  (reduce apply-op (clear-matches R) (apply concat (get-matches R))))

(defn add-until-stable
  ;; depth-first
  ([R ops] (add-until-stable R ops 100))
  ([R ops max-iterations]
     (assert (empty? (get-matches R)))
     (loop [R'     R
            open   (seq ops)
            closed []
            safety 0]
       (assert (< safety max-iterations) (str "safety:" safety ", open:" open))
       (if (empty? open)
         [R' closed]
         (let [[op & ops] open
               R''        (apply-op R' op)
               successors (apply concat (get-matches R''))]
           (recur (clear-matches R'')
                  (concat successors ops)
                  (conj closed op)
                  (inc safety)))))))
