(ns com.pettomato.rete.builder
  (:require
   [clojure.walk :refer (postwalk postwalk-replace)]
   [com.pettomato.rete :refer [var-symbol? alpha-test-node alpha-mem-node beta-mem-node join-node production-node]]))

(defn aggregate-consistency-tests [r]
  ;; ABxyCz => (A)(Bxy)(Cz)m where ABC are predicates and xyz are
  ;; disequality constraints. The disequality constraints are
  ;; associated with the preceding predicate.
  (loop [cs r, acc []]
    (if (empty? cs)
      acc
      (let [[c & cs'] cs
            [ds cs''] (split-with list? cs')]
        (recur cs'' (conj acc (conj ds c)))))))

(defn condition->alpha-tests [c]
  (->> (map-indexed #(vector '= %1 %2) c)
       (remove (comp var-symbol? last))))

(defn condition->equality-tests [ri c vars]
  (->> (map-indexed #(vector [ri %1] %2) c)
       (filter (comp var-symbol? second))
       (map (fn [[pos v]] (list #+clj = #+cljs '= [::pos pos] (get vars v))))
       (remove (fn [[op p1 p2]] (= p1 p2)))))

(defn canonicalize-tests [r sort-alpha-tests]
  (let [r' (aggregate-consistency-tests r)
        vars (->> r'
                  (map first)
                  (map-indexed (fn [ri c] (map-indexed (fn [ci v] [v [::pos [ri ci]]]) c)))
                  (apply concat)
                  (filter (comp var-symbol? first))
                  reverse
                  (into {}))
        alpha-tests (->> (map first r')
                         (map condition->alpha-tests)
                         (map sort-alpha-tests))
        equality-tests (->> (map first r')
                            (map-indexed #(condition->equality-tests %1 %2 vars)))
        disequality-tests (->> (map rest r')
                               (postwalk-replace vars))]
    (map vector alpha-tests equality-tests disequality-tests)))

(defn build-nodes [rs sort-alpha-tests]
  (let [rs             (map #(update-in % [:conditions] canonicalize-tests sort-alpha-tests) rs)
        cs             (map :conditions rs)
        alpha-sigs     (into #{} (for [r cs, c r, x (iterate butlast (first c)) :while x] x))
        beta-sigs      (into #{} (for [r cs,      x (iterate butlast r)         :while x] x))
        ids            (fn [prefix] (map #(keyword (str prefix %)) (range)))
        alpha-mem-ids  (zipmap     alpha-sigs (ids "a"))
        alpha-test-ids (zipmap     alpha-sigs (ids "a-test"))
        beta-mem-ids   (-> (zipmap beta-sigs  (ids "b"))
                           (assoc nil :dummy))
        join-ids       (zipmap     beta-sigs  (ids "j"))]
    {:alpha-tests (reduce #(let [k (alpha-test-ids %2)
                                 v {:parent (alpha-mem-ids (butlast %2))
                                    :test   (last %2)}]
                             (assoc %1 k v))
                          {}
                          alpha-sigs)
     :alpha-mems  (reduce #(let [k (alpha-mem-ids %2)
                                 v {:parent (alpha-test-ids %2)
                                    :memory #{}}]
                             (assoc %1 k v))
                          {}
                          alpha-sigs)
     :beta-mems   (-> (zipmap (vals beta-mem-ids) (repeat {:memory #{}}))
                      (assoc :dummy {:memory #{[]}}))
     :joins       (zipmap (vals join-ids)
                          (map (fn [b]
                                 {:alpha (alpha-mem-ids (first (last b)))
                                  :beta  (beta-mem-ids (butlast b))
                                  :tests (apply concat (rest (last b)))
                                  :out   (beta-mem-ids b)})
                               beta-sigs))
     :productions (reduce (fn [m [k r]]
                            (let [v {:parent (beta-mem-ids (:conditions r))
                                     :conditions (:conditions r)
                                     :add-matches (:add-matches r)
                                     :rem-matches (:rem-matches r)}]
                              (assoc m k v)))
                          {}
                          (map vector (ids "p") rs))}))

(defn build-graph [nodes]
  (let [{:keys [alpha-tests alpha-mems beta-mems joins productions]} nodes
        f (fn [parent m k v] (update-in m [(parent v) :successors] conj k))
        alpha-tests' (reduce-kv (partial f :parent) alpha-tests alpha-mems)
        alpha-mems'  (-> (reduce-kv (partial f :parent) alpha-mems alpha-tests)
                         (dissoc nil))
        alpha-mems'  (reduce-kv (partial f :alpha) alpha-mems' joins)
        beta-mems'   (-> (reduce-kv (partial f :beta) beta-mems joins)
                         (dissoc nil))
        beta-mems'   (reduce-kv (partial f :parent) beta-mems' productions)]
    {:alpha-tests alpha-tests'
     :alpha-mems  alpha-mems'
     :beta-mems   beta-mems'
     :joins       joins
     :productions productions}))

(defn optimize-graph [g]
  g)

(defn compile-constant-test [[op pos val]]
  (assert (= op '=))
  #(= (get % pos) val))

(defn tagged-pos? [x] (and (vector? x) (= (first x) ::pos)))

#+cljs
(def allowed-ops {'= =, 'not= not=, '< <, '<= <=, '> >, '>= >=, '+ +, '- -, '/ /, '* *})

#+cljs
(defn eval-expr [e]
  (if (and (coll? e) (fn? (first e)))
    (apply (first e) (map eval-expr (rest e)))
    e))

(defn compile-consistency-tests [tests]
  (let [tests #+clj tests #+cljs (postwalk-replace allowed-ops tests)]
    (fn [t w]
      (let [t' (conj t w)
            expr (postwalk #(if (tagged-pos? %) (get-in t' (second %)) %)
                                        tests)]
        (every? true? (map #+clj eval #+cljs eval-expr expr))))))

(defn compile-graph [g]
  (let [{:keys [alpha-tests alpha-mems beta-mems joins productions]} g
        alpha-tests' (reduce-kv (fn [m k v]
                                  (let [{:keys [test successors]} v
                                        test' (compile-constant-test test)]
                                    (assoc m k (alpha-test-node test' successors))))
                                {}
                                alpha-tests)
        alpha-mems'  (reduce-kv (fn [m k v]
                                  (let [{:keys [memory successors]} v]
                                    (assoc m k (alpha-mem-node memory successors))))
                                {}
                                alpha-mems)
        beta-mems'   (reduce-kv (fn [m k v]
                                  (let [{:keys [memory successors]} v]
                                    (assoc m k (beta-mem-node memory successors))))
                                {}
                                beta-mems)
        joins'       (reduce-kv (fn [m k v]
                                  (let [{:keys [alpha beta tests out]} v
                                        test (compile-consistency-tests tests)]
                                    (assoc m k (join-node alpha beta test #{out}))))
                                {}
                                joins)
        productions' (reduce-kv (fn [m k v]
                                  (assoc m k (production-node (:add-matches v) (:rem-matches v))))
                                {}
                                productions)
        entry-nodes  (reduce-kv (fn [m k v]
                                  (if (nil? (:parent v))
                                    (assoc m k v)
                                    m))
                                {}
                                alpha-tests)]
    {:entry-nodes entry-nodes
     :matches     []
     :nodes       (merge alpha-tests' alpha-mems' beta-mems' joins' productions')}))

(defn entry-nodes->field-lookup [entry-nodes field]
  (reduce-kv (fn [m k v]
               (let [[op pos val] (:test v)]
                 (if (= pos field)
                   (assoc m val k)
                   m)))
             {}
             entry-nodes))

(defn hash-entry-nodes-by-field [g field]
  (let [a->k (entry-nodes->field-lookup (:entry-nodes g) field)]
    (-> g
        (assoc :root-fn (fn [w] (a->k (get w field))))
        (dissoc :entry-nodes))))

(defn default-alpha-sort [index-field]
  (fn [x]
    (sort-by second #(cond (= %1 index-field) -1
                           (= %2 index-field) 1
                           :else              (compare %1 %2))
             x)))

(defn parse-and-compile-rules [index-field rs]
  (-> rs
      (build-nodes (default-alpha-sort index-field))
      build-graph
      optimize-graph
      compile-graph
      (hash-entry-nodes-by-field index-field)))
