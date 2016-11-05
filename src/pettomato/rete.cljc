(ns pettomato.rete)

;; R : A compiled Rete network. {:keys [root-fn nodes alpha-mem beta-mem matches]}.
;; w : A Working Memory Element (wme).
;; t : A token, which is a seq of working memory elements.
;; m : A match, which is a complete token.

(defn memoize-once
  "Like memoize, but forgets cached mappings after they are used
  once. This is an optimization used to avoid a redundant computation
  when removing a match."
  ;; Used in rete-macros.
  [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (do (swap! mem dissoc args)
            (val e))
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

(defn collapse-matches [matches]
  (reduce (fn [acc [op match]]
            (let [complement [(case op :+ :- :- :+) match]]
              (if (some #{complement} acc)
                (vec (remove #{complement} acc))
                (conj acc [op match]))))
          []
          matches))

(defn invert-signed-terms [res]
  "Takes a seq of signed terms and returns a new sequence that is the
   the inversion of the original. All signs will be flipped and the
   order of terms will be reversed."
  ;; Used in rete-macros.
  ;; Note that the order of terms is reversed implicitly by consing
  ;; onto a list.
  (reduce (fn [l [op v]]
            (cons (case op
                    :- [:+ v]
                    :+ [:- v])
                  l))
          ()
          res))

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
