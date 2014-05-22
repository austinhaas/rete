(ns com.pettomato.rete.builder
  (:require
   [clojure.walk :refer (postwalk postwalk-replace)]
   [com.pettomato.rete :refer [var-symbol?]]))

(defn condition->alpha-tests [c]
  (->> (map-indexed #(vector '= %1 %2) c)
       (remove (comp var-symbol? peek))))

(defn condition->equality-tests [ri c vars]
  (->> (map-indexed #(vector [ri %1] %2) c)
       (filter (comp var-symbol? second))
       (map (fn [[pos v]] (vector '= pos (get vars v))))
       (remove (fn [[op p1 p2]] (= p1 p2)))))

(defn canonicalize-tests [r]
  (let [var->pos    (->> r
                         (map-indexed (fn [ri c] (map-indexed (fn [ci v] [v [ri ci]]) c)))
                         (apply concat)
                         (filter (comp var-symbol? first))
                         reverse
                         (into {}))
        alpha-tests (map condition->alpha-tests r)
        equal-tests (map-indexed #(condition->equality-tests %1 %2 var->pos) r)]
    (map vector alpha-tests equal-tests)))

(defn build-nodes [rs]
  (let [rs (map (fn [r] (update-in r [:conditions] canonicalize-tests)) rs)
        dummy {:type   :beta-mem
               :flag   :dummy
               :id     []
               :parent nil
               :tests  nil}]
    (loop [rs    rs
           nodes []]
      (if (empty? rs)
        (distinct nodes)
        (let [[r & rs']     rs
              [nodes' beta] (loop [cs    (:conditions r)
                                   nodes nodes
                                   beta  dummy]
                              (if (empty? cs)
                                [nodes beta]
                                (let [[c & cs'] cs
                                      [a b]     c ;; a = constant tests, b = consistency tests.
                                      alphas (loop [tests  a
                                                    acc    ()]
                                               (if (empty? tests)
                                                 acc
                                                 (let [[t & ts]  tests
                                                       parent    (vec (:id (first acc)))
                                                       id        (conj parent t b)
                                                       test-node {:type   :alpha-test
                                                                  :parent parent
                                                                  :id     id
                                                                  :test   t}
                                                       mem-node  {:type   :alpha-mem
                                                                  :parent id
                                                                  :id     id
                                                                  :tests  b}
                                                       acc'     (conj acc test-node mem-node)]
                                                   (recur ts acc'))))
                                      beta'  (if (empty? b)
                                               beta
                                               (dissoc (assoc beta :tests b :id (conj (:id beta) b)) :flag))
                                      join   (let [alpha-id (:id (first alphas))
                                                   beta-id  (:id beta')]
                                               {:type  :join
                                                :id    (conj beta-id c)
                                                :alpha alpha-id
                                                :beta  beta-id
                                                :tests b})
                                      b-out  {:type   :beta-mem
                                              :parent (:id join)
                                              :id     (:id join)
                                              :tests  nil}
                                      nodes' (reverse (conj alphas beta' join))]
                                  (recur cs' (into nodes nodes') b-out))))
              prod   {:type        :production
                      :parent      (:id beta)
                      :id          (gensym)
                      :conditions  (:conditions r)
                      :add-matches (:add-matches r)
                      :rem-matches (:rem-matches r)}]
          (recur rs' (conj nodes' beta prod)))))))

(defn index-nodes [nodes]
  (let [sig->id (zipmap (map (juxt :type :id) nodes) (range))
        nodes'  (map (fn [n] (assoc n :id (sig->id ((juxt :type :id) n)))) nodes)
        i-nodes (vec (sort-by :id nodes'))]
    (mapv (fn [node]
            (case (:type node)
              :alpha-test (update-in node [:parent] #(get sig->id [:alpha-mem %]))
              :alpha-mem  (update-in node [:parent] #(get sig->id [:alpha-test %]))
              :beta-mem   (update-in node [:parent] #(get sig->id [:join %]))
              :join       (-> node
                              (update-in [:alpha] #(get sig->id [:alpha-mem %]))
                              (update-in [:beta]  #(get sig->id [:beta-mem %])))
              :production (update-in node [:parent] #(get sig->id [:beta-mem %]))))
          i-nodes)))

(defn add-successor-edges [nodes]
  (reduce (fn [ns n]
            (case (:type n)
              :join (-> ns
                        (update-in [(:alpha n) :successors] (fnil conj #{}) (:id n))
                        (update-in [(:beta n)  :successors] (fnil conj #{}) (:id n)))
              (if (:parent n)
                (update-in ns [(:parent n) :successors] (fnil conj #{}) (:id n))
                ns)))
          nodes
          nodes))

(defn add-key-fns [nodes]
  (mapv (fn [n]
          (case (:type n)
            :alpha-test n
            :alpha-mem  (let [id     (:id n)
                              tests  (:tests n)
                              ps     (map #(get-in % [1 1]) tests)
                              key-fn (fn [w]
                                       (let [ks (map #(get w %) ps)]
                                         (list* :alpha-mem id ks)))]
                          (assoc n :key-fn key-fn))
            :beta-mem   (let [id     (:id n)
                              tests  (:tests n)
                              ps     (map #(get % 2) tests)
                              key-fn (fn [t]
                                       (let [ks (map #(get-in t %) ps)]
                                         (list* :beta-mem id ks)))]
                          (assoc n :key-fn key-fn))
            :join       (let [id           (:id n)
                              tests        (:tests n)
                              alpha-id     (:alpha n)
                              beta-id      (:beta n)
                              ps1          (map #(get % 2) tests)
                              alpha-key-fn (fn [t]
                                             (let [ks (map #(get-in t %) ps1)]
                                               (list* :alpha-mem alpha-id ks)))
                              ps2          (map #(get-in % [1 1]) tests)
                              beta-key-fn  (fn [w]
                                             (let [ks (map #(get w %) ps2)]
                                               (list* :beta-mem beta-id ks)))]
                          (assoc n :alpha-key-fn alpha-key-fn :beta-key-fn beta-key-fn))
            :production n))
        nodes))

(defn compile-constant-test [[op pos val]]
  (assert (= op '=))
  #(= (get % pos) val))

(defn compile-nodes [nodes]
  (let [entry-ns (->> nodes
                      (filter #(= (:type %) :alpha-test))
                      (filter #(nil? (:parent %))))
        a->k     (reduce (fn [m n]
                           (let [[op pos val] (:test n)]
                             (if (= pos 0)
                               (update-in m [val] (fnil conj #{}) (:id n))
                               m)))
                         {}
                         entry-ns)
        nodes'   (mapv (fn [n]
                         (case (:type n)
                           :alpha-test (update-in n [:test] compile-constant-test)
                           n))
                       nodes)
        dummy-id (:id (first (filter #(= (:flag %) :dummy) nodes)))]
    {:root-fn   (comp a->k first)
     :nodes     nodes'
     :alpha-mem {}
     :beta-mem  {dummy-id #{[]}}
     :matches   []}))

(defn parse-and-compile-rules [rs]
  (let [index-field 0]
    (-> rs
        build-nodes
        index-nodes
        add-successor-edges
        add-key-fns
        compile-nodes)))
