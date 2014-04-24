(ns com.pettomato.rete.builder
  (:require
   [com.pettomato.rete :refer [alpha-test-node alpha-mem-node beta-mem-node join-node production-node]]))

(defn var-symbol? [x]
  (and (symbol? x)
       (= (get (str x) 0) \?)))

(defn get-alpha-tests [r]
  (map (fn [c]
         (remove nil?
                 (map-indexed (fn [i t]
                                (if (var-symbol? t)
                                  nil
                                  ['= i t]))
                              c)))
       r))

(defn get-consistency-tests [r]
  (let [vars (->> (map-indexed (fn [ri c]
                                 (map-indexed (fn [ci t] [t [ri ci]])
                                              c))
                               r)
                  (apply concat)
                  (filter (comp var-symbol? first)))
        tests (->> (map (fn [[v pos]] ['= (second (first (filter #(= (first %) v) vars))) pos]) vars)
                   (remove (fn [[op a b]] (= a b)))
                   (map (fn [[op a [ri ci]]] [ri [op a ci]]))
                   (group-by first)
                   (reduce-kv (fn [m k v] (assoc m k (map second v))) {}))]
    (map-indexed (fn [i c] (get tests i)) r)))

(defn construct-tests [r]
  (map vector
       (get-alpha-tests r)
       (get-consistency-tests r)))

(defn build-nodes [rs]
  (let [rs             (map #(update-in % [:conditions] construct-tests) rs)
        cs             (map :conditions rs)
        alpha-sigs     (for [r cs, c r, x (take-while identity (iterate butlast (first c)))] x)
        beta-sigs      (for [r cs, x (take-while identity (iterate butlast r))] x)

        alpha-mem-ids  (zipmap alpha-sigs (map keyword (map str (repeat "a") (range))))
        alpha-test-ids (zipmap alpha-sigs (map keyword (map str (repeat "a-test") (range))))
        beta-mem-ids   (assoc (zipmap beta-sigs (map keyword (map str (repeat "b") (range))))
                         nil :dummy)
        join-ids   (zipmap beta-sigs (map keyword (map str (repeat "j") (range))))]
    {:alpha-tests (into {} (for [a alpha-sigs]
                             [(alpha-test-ids a) {:parent (alpha-mem-ids (butlast a))
                                                  :test   (last a)}]))
     :alpha-mems  (into {} (for [a alpha-sigs]
                             [(alpha-mem-ids a) {:parent (alpha-test-ids a)
                                                 :memory #{}}]))
     :beta-mems   (assoc (zipmap (vals beta-mem-ids) (repeat {:memory #{}}))
                    :dummy {:memory #{[]}})
     :joins       (zipmap (vals join-ids)
                          (map (fn [b]
                                 {:alpha (alpha-mem-ids (first (last b)))
                                  :beta  (beta-mem-ids (butlast b))
                                  :tests (second (last b))
                                  :out   (beta-mem-ids b)})
                               beta-sigs))
     :productions (into {} (map-indexed #(vector (keyword (str "p" %1))
                                                 {:parent (beta-mem-ids (:conditions %2))
                                                  :conditions (:conditions %2)
                                                  :add-matches (:add-matches %2)
                                                  :rem-matches (:rem-matches %2)})
                                        rs))}))

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

(defn compile-consistency-tests [tests]
  (fn [t w]
    (every? true? (for [[op p1 p2] tests]
                    (= (get-in (conj t w) p1) (get w p2))))))

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

(defn parse-and-compile-rules [index-field rs]
  (-> rs
      build-nodes
      build-graph
      optimize-graph
      compile-graph
      (hash-entry-nodes-by-field index-field)))
