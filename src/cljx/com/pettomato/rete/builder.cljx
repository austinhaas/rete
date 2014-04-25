(ns com.pettomato.rete.builder
  (:require
   [com.pettomato.rete :refer [alpha-test-node alpha-mem-node beta-mem-node join-node production-node]]))

(defn var-symbol? [x]
  (and (symbol? x)
       (= (get (str x) 0) \?)))

(defn get-alpha-tests [r sort-alpha-tests]
  (map (fn [c]
         (sort-alpha-tests
          (remove nil?
                  (map-indexed (fn [i t]
                                 (if (var-symbol? t)
                                   nil
                                   ['= i t]))
                               c))))
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

(defn construct-tests [r sort-alpha-tests]
  (map vector
       (get-alpha-tests r sort-alpha-tests)
       (get-consistency-tests r)))

(defn build-nodes [rs sort-alpha-tests]
  (let [rs             (map #(update-in % [:conditions] construct-tests sort-alpha-tests) rs)
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
                                  :tests (second (last b))
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
      (build-nodes (fn [x] (sort-by second #(cond (= %1 index-field) -1
                                                  (= %2 index-field) 1
                                                  :else    (compare %1 %2))
                                    x)))
      build-graph
      optimize-graph
      compile-graph
      (hash-entry-nodes-by-field index-field)))

(defn build-smap [var-lookup match]
  (reduce-kv #(assoc %1 %2 (get-in match %3)) {} var-lookup))

(defn action->rule [action-schema]
  (let [{:keys [preconditions achieves deletes]} action-schema
        vars (->> (map-indexed (fn [ri c]
                                 (map-indexed (fn [ci t] [t [ri ci]])
                                              c))
                               preconditions)
                  (apply concat)
                  (filter (comp var-symbol? first))
                  (into {}))
        add-template (concat (for [x deletes]  ['- x])
                             (for [x achieves] ['+ x]))
        rem-template (concat (for [x achieves] ['- x])
                             (for [x deletes]  ['+ x]))]
    {:conditions  preconditions
     :add-matches #(mapcat (fn [match] (clojure.walk/postwalk-replace (build-smap vars match) add-template)) %)
     ;;:rem-matches #(mapcat (fn [match] (clojure.walk/postwalk-replace (build-smap vars match) rem-template)) %)
     :rem-matches (constantly [])
     }))
