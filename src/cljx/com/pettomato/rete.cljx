(ns com.pettomato.rete)

;; R : A compiled Rete network. {:keys [root-fn nodes]}.
;; w : A Working Memory Element (wme).
;; t : A token, which is a seq of working memory elements.
;; m : A match, which is a complete token.
;; k : An indirect reference to a node or memory location.

(defn alpha-test-node [constant-test successors]
  {:type          :alpha-test
   :constant-test constant-test
   :successors    successors})

(defn alpha-mem-node [mem successors]
  {:type       :alpha-mem
   :memory     mem
   :add-wme    conj
   :rem-wme    disj
   :successors successors})

(defn beta-mem-node [mem successors]
  {:type       :beta-mem
   :memory     mem
   :add-tokens into
   :rem-tokens #(apply disj %1 %2)
   :successors successors})

(defn join-node [alpha-k beta-k consistency-test successors]
  {:type             :join
   :consistency-test consistency-test
   :alpha-k          alpha-k
   :beta-k           beta-k
   :successors       successors})

(defn production-node [f f-]
  {:type        :production
   :add-matches f
   :rem-matches f-})

(defn get-node [R k] (get-in R [:nodes k]))

(defn get-alpha [R k] (get-in R [:alpha-mem k]))
(defn put-alpha [R k v] (assoc-in R [:alpha-mem k] v))

(defn get-beta [R k] (get-in R [:beta-mem k]))
(defn put-beta [R k v] (assoc-in R [:beta-mem k] v))

(defn put-matches [R ms] (update-in R [:matches] conj ms))

(defn left-activate [R k ts]
  (let [node (get-node R k)]
    (case (:type node)
      :beta-mem   (let [f    (:add-tokens node)
                        ts'  (or (get-beta R k) (:memory node))
                        ts'' (f ts' ts)
                        R'   (put-beta R k ts'')]
                    (reduce #(left-activate %1 %2 ts) R' (:successors node)))
      :join       (let [f       (:consistency-test node)
                        alpha-k (:alpha-k node)
                        ws      (or (get-alpha R alpha-k) (:memory (get-node R alpha-k)))
                        ts'     (for [t ts, w ws :when (f t w)] (conj t w))]
                    (if (empty? ts')
                      R
                      (reduce #(left-activate %1 %2 ts') R (:successors node))))
      :production (let [f (:add-matches node)]
                    (put-matches R (f ts))))))

(defn right-activate [R k w]
  (let [node (get-node R k)]
    (case (:type node)
      :alpha-test (let [f (:constant-test node)]
                    (if (f w)
                      (reduce #(right-activate %1 %2 w) R (:successors node))
                      R))
      :alpha-mem  (let [f   (:add-wme node)
                        ws  (or (get-alpha R k) (:memory node))
                        ws' (f ws w)
                        R'  (put-alpha R k ws')]
                    (reduce #(right-activate %1 %2 w) R' (:successors node)))
      :join       (let [f      (:consistency-test node)
                        beta-k (:beta-k node)
                        ts     (or (get-beta R beta-k) (:memory (get-node R beta-k)))
                        ts'    (for [t ts :when (f t w)] (conj t w))]
                    (if (empty? ts')
                      R
                      (reduce #(left-activate %1 %2 ts') R (:successors node)))))))

(defn left-activate- [R k ts]
  (let [node (get-node R k)]
    (case (:type node)
      :beta-mem   (let [f    (:rem-tokens node)
                        ts'  (or (get-beta R k) (:memory node))
                        ts'' (f ts' ts)
                        R'   (put-beta R k ts'')]
                    (reduce #(left-activate- %1 %2 ts) R' (:successors node)))
      :join       (let [f       (:consistency-test node)
                        alpha-k (:alpha-k node)
                        ws      (or (get-alpha R alpha-k) (:memory (get-node R alpha-k)))
                        ts'     (for [t ts, w ws :when (f t w)] (conj t w))]
                    (if (empty? ts')
                      R
                      (reduce #(left-activate- %1 %2 ts') R (:successors node))))
      :production (let [f (:rem-matches node)]
                    (put-matches R (f ts))))))

(defn right-activate- [R k w]
  (let [node (get-node R k)]
    (case (:type node)
      :alpha-test (let [f (:constant-test node)]
                    (if (f w)
                      (reduce #(right-activate- %1 %2 w) R (:successors node))
                      R))
      :alpha-mem  (let [f   (:rem-wme node)
                        ws  (or (get-alpha R k) (:memory node))
                        ws' (f ws w)
                        R'  (put-alpha R k ws')]
                    (reduce #(right-activate- %1 %2 w) R' (:successors node)))
      :join       (let [f      (:consistency-test node)
                        beta-k (:beta-k node)
                        ts     (or (get-beta R beta-k) (:memory (get-node R beta-k)))
                        ts'    (for [t ts :when (f t w)] (conj t w))]
                    (if (empty? ts')
                      R
                      (reduce #(left-activate- %1 %2 ts') R (:successors node)))))))

;;; API

(defn add-wme [R w]
  (let [successor-fn (:root-fn R)]
    (if-let [k (successor-fn w)]
      (right-activate R k w)
      R)))

(defn remove-wme [R w]
  (let [successor-fn (:root-fn R)]
    (if-let [k (successor-fn w)]
      (right-activate- R k w)
      R)))

(defn get-matches [R] (get R :matches))

(defn clear-matches [R] (update-in R [:matches] empty))
