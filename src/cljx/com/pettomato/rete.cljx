(ns com.pettomato.rete)

;; R : A compiled Rete network. {:keys [root-fn nodes alpha-mem beta-mem matches]}.
;; w : A Working Memory Element (wme).
;; t : A token, which is a seq of working memory elements.
;; m : A match, which is a complete token.
;; k : An indirect reference to a node.

(defn update
  "Updates the value in map m at k with the function f.

  Like update-in, but for updating a single top-level key.
  Any additional args will be passed to f after the value."
  ([m k f] (assoc m k (f (get m k))))
  ([m k f x1] (assoc m k (f (get m k) x1)))
  ([m k f x1 x2] (assoc m k (f (get m k) x1 x2)))
  ([m k f x1 x2 & xs] (assoc m k (apply f (get m k) x1 x2 xs))))

(defn var-symbol? [x]
  (and (symbol? x)
       (= (get (str x) 0) \?)))

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
      :production (let [add-matches (:add-matches node)
                        ms          (add-matches ts)]
                    (if (empty? ms)
                      R
                      (update R :matches conj ms))))))

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
      :production (let [rem-matches (:rem-matches node)
                        ms          (rem-matches ts)]
                    (if (empty? ms)
                      R
                      (update R :matches conj ms))))))

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

(defn get-matches [R] (get R :matches))

(defn clear-matches [R] (update R :matches empty))
