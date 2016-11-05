(ns pettomato.rete)

;; R : A compiled Rete network. {:keys [root-fn nodes alpha-mem beta-mem matches]}.
;; w : A Working Memory Element (wme).
;; t : A token, which is a seq of working memory elements.
;; m : A match, which is a complete token.

(defn has-matches?
  "Returns true if any of the rete's productions have been activated
  with a match."
  [R]
  (boolean (seq (:activated-productions R))))

(defn trigger-next
  "Triggers the next activated production. Returns a pair of a new
  rete and the output from the production."
  [R]
  (let [[p id] (first (:activated-productions R))
        f      ((:productions R) id)
        ms     (get-in R [:matches id])
        R'     (-> R
                   (update :activated-productions disj [p id])
                   (update-in [:matches id] empty))
        out    (f ms)]
    [R' out]))

(defn add-wme
  "Add a working memory element to the rete."
  [R w]
  ((:add-wme R) R w))

(defn remove-wme
  "Remove a working memory element from the rete."
  [R w]
  ((:rem-wme R) R w))

(defn- apply-op [R [op w]]
  (case op
    :+ (add-wme R w)
    :- (remove-wme R w)))

(defn add-until-stable
  "Add a sequence of signed working memory elements, ops, to the
  rete. Any productions activated will be triggered and the output
  from these productions will be added to the rete as new working
  memory elements. This cycle will continue until the system
  stabilizes (i.e., no productions are activated) or max-iterations is
  reached."
  ([R ops] (add-until-stable R ops 100))
  ([R ops max-iterations]
   ;; Note that multiple productions could be activated at the same
   ;; time, but the one with the highest priority will be triggered
   ;; first, and its output will be fed back into the system BEFORE
   ;; any lower priority productions are triggered. That may cause NEW
   ;; productions to become activated with a higher priority than
   ;; those ALREADY activated, and thus, they will fire first. This is
   ;; like a "best-first" search.
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
