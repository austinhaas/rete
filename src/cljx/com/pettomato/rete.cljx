(ns com.pettomato.rete
  (:require
   [com.pettomato.rete.util :refer [update]]))

;; R : A compiled Rete network. {:keys [root-fn nodes alpha-mem beta-mem matches]}.
;; w : A Working Memory Element (wme).
;; t : A token, which is a seq of working memory elements.
;; m : A match, which is a complete token.
;; k : An indirect reference to a node.

(defn get-node [R k] ((:nodes R) k))

(defn left-activate [R k ts]
  (let [node (get-node R k)]
    (case (:type node)
      :beta-mem   (let [key-fn (:key-fn node)
                        R'     (reduce #(update-in %1 (key-fn %2) (fnil conj #{}) %2) R ts)]
                    (reduce #(left-activate %1 %2 ts) R' (:successors node)))
      :join       (let [key-fn (:alpha-key-fn node)
                        ts'    (for [t ts, w (get-in R (key-fn t))] (conj t w))]
                    (cond
                     (empty? ts') R
                     :else        (reduce #(left-activate %1 %2 ts') R (:successors node))))
      :production (let [id (:id node)
                        priority (:priority node)
                        ms (map #(vector :+ %) ts)]
                    (-> (update-in R [:matches id] (fnil into []) ms)
                        (update-in [:activated-productions] conj [priority id]))))))

(defn right-activate [R k w]
  (let [node (get-node R k)]
    (case (:type node)
      :alpha-test (let [test (:test node)]
                    (cond
                     (test w) (reduce #(right-activate %1 %2 w) R (:successors node))
                     :else    R))
      :alpha-mem  (let [key-fn (:key-fn node)
                        R'     (update-in R (key-fn w) (fnil conj #{}) w)]
                    (reduce #(right-activate %1 %2 w) R' (:successors node)))
      :join       (let [key-fn (:beta-key-fn node)
                        ts     (get-in R (key-fn w))
                        ts'    (for [t ts] (conj t w))]
                    (cond
                     (empty? ts') R
                     :else        (reduce #(left-activate %1 %2 ts') R (:successors node)))))))

(defn left-activate- [R k ts]
  (let [node (get-node R k)]
    (case (:type node)
      :beta-mem   (let [key-fn (:key-fn node)
                        R'     (reduce #(update-in %1 (key-fn %2) (fnil disj #{}) %2) R ts)]
                    (reduce #(left-activate- %1 %2 ts) R' (:successors node)))
      :join       (let [key-fn (:alpha-key-fn node)
                        ts'    (for [t ts, w (get-in R (key-fn t))] (conj t w))]
                    (cond
                     (empty? ts') R
                     :else        (reduce #(left-activate- %1 %2 ts') R (:successors node))))
      :production (let [id (:id node)
                        priority (:priority node)
                        ms (map #(vector :- %) ts)]
                    (-> (update-in R [:matches id] (fnil into []) ms)
                        (update-in [:activated-productions] conj [priority id]))))))

(defn right-activate- [R k w]
  (let [node (get-node R k)]
    (case (:type node)
      :alpha-test (let [test (:test node)]
                    (cond
                     (test w) (reduce #(right-activate- %1 %2 w) R (:successors node))
                     :else    R))
      :alpha-mem  (let [key-fn (:key-fn node)
                        R'     (update-in R (key-fn w) (fnil disj #{}) w)]
                    (reduce #(right-activate- %1 %2 w) R' (:successors node)))
      :join       (let [key-fn (:beta-key-fn node)
                        ts     (get-in R (key-fn w))
                        ts'    (for [t ts] (conj t w))]
                    (cond
                     (empty? ts') R
                     :else        (reduce #(left-activate- %1 %2 ts') R (:successors node)))))))

(defn has-matches? [R]
  (not (empty? (:activated-productions R))))

(defn trigger-next [R]
  (let [[p id] (first (:activated-productions R))
        node   (get-node R id)
        f      (:fn node)
        ms     (get-in R [:matches id])
        R'     (-> R
                   (update-in [:activated-productions] disj [p id])
                   (update-in [:matches id] empty))
        out    (f ms)]
    [R' out]))

;;; API

(defn add-wme [R w]
  (let [successor-fn (:root-fn R)]
    (if-let [ks (successor-fn w)]
      (reduce #(right-activate %1 %2 w) R ks)
      R)))

(defn remove-wme [R w]
  (let [successor-fn (:root-fn R)]
    (if-let [ks (successor-fn w)]
      (reduce #(right-activate- %1 %2 w) R ks)
      R)))
