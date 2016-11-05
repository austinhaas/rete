(ns pettomato.rete)

;; R : A compiled Rete network. {:keys [root-fn nodes alpha-mem beta-mem matches]}.
;; w : A Working Memory Element (wme).
;; t : A token, which is a seq of working memory elements.
;; m : A match, which is a complete token.

(defn has-matches? [R]
  (not (empty? (:activated-productions R))))

(defn trigger-next [R]
  (let [[p id] (first (:activated-productions R))
        f      ((:productions R) id)
        ms     (get-in R [:matches id])
        R'     (-> R
                   (update :activated-productions disj [p id])
                   (update-in [:matches id] empty))
        out    (f ms)]
    [R' out]))

(defn add-wme [R w]
  ((:add-wme R) R w))

(defn remove-wme [R w]
  ((:rem-wme R) R w))

(defn apply-op [R [op w]]
  (case op
    :+ (add-wme R w)
    :- (remove-wme R w)))

(defn add-until-stable
  ([R ops] (add-until-stable R ops 100))
  ([R ops max-iterations]
   (loop [R      R
          ops    (seq ops)
          acc    []
          safety 0]
     (assert (< safety max-iterations) (str "safety:" safety))
     (let [R' (reduce apply-op R ops)]
       (if (has-matches? R')
         (let [[R'' out] (trigger-next R')]
           (recur R'' out (into acc out) (inc safety)))
         [R' acc])))))
