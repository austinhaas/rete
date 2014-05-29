(ns com.pettomato.rete.rete-macros
  (:require
   [clojure.walk :refer [postwalk]]
   [com.pettomato.rete :refer [update memoize-once collapse-matches invert-match-result]]))

(defn var-symbol? [x] (and (symbol? x) (= (get (str x) 0) \?)))

(defn expr? [x] (and (list? x) (symbol? (first x))))

(defn synthesize-production [preconds achieves deletes cache? inv-match]
  (let [;; 'var->pos' is a mapping from each variable in preconditions to
        ;; the position where it first appears.
        ;;   e.g., [[?a 'x] [?b 'y]] => {?a [0 0], ?b [1 0]}.
        var->pos  (->> preconds
                       (map-indexed (fn [ri c] (map-indexed (fn [ci t] [t [ri ci]]) c)))
                       (apply concat)
                       (filter (comp var-symbol? first))
                       reverse    ;; So that earlier bindings will replace later bindings.
                       (into {}))
        match     (gensym)
        bindings  (atom [])
        expr->var (atom {})
        template  (->> (concat (for [x deletes]  [:- x])
                               (for [x achieves] [:+ x]))
                       (postwalk #(cond
                                   (contains? var->pos %) (let [v    %
                                                                pos  (get var->pos v)
                                                                expr `(get-in ~match ~pos)]
                                                            (swap! bindings into [v expr])
                                                            v)
                                   (expr? %)              (let [v    (@expr->var % (gensym))
                                                                expr %]
                                                            (swap! bindings into [v expr])
                                                            (swap! expr->var assoc % v)
                                                            v)
                                   :else                  %))
                       vec)
        bindings  (distinct @bindings)
        inv-match (if (true? inv-match)
                    `invert-match-result
                    inv-match)
        add       (gensym)
        rem       (gensym)]
    `(let [~add (fn [~match]
                  (let [~@bindings]
                    ~template))
           ~add ~(if cache? `(memoize-once ~add) add)
           ~rem (if ~inv-match
                  (comp ~inv-match ~add)
                  (constantly []))]
       (fn [matches#]
         (reduce (fn [v# [op# match#]]
                   (case op#
                     :+ (into v# (~add match#))
                     :- (into v# (~rem match#))))
                 []
                 matches#)))))

(defn canonicalize-rule [r]
  (let [r'  (if (contains? r :fn)
              r
              (let [{:keys [preconds achieves deletes cache? inv-match]} r]
                (-> r
                    (assoc :fn (synthesize-production preconds achieves deletes cache? inv-match))
                    (dissoc :achieves :deletes :cache? :inv-match))))
        r'' (if (get r' :collapse-matches true)
              (update-in r' [:fn] #(list `comp % `collapse-matches))
              r')]
    r''))

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
  (let [rs (map (fn [r] (update r :preconds canonicalize-tests)) rs)
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
              [nodes' beta] (loop [cs    (:preconds r)
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
              prod          {:type     :production
                             :parent   (:id beta)
                             :id       (gensym)
                             :priority (:priority r)
                             :preconds (:preconds r)
                             :fn       (:fn r)}]
          (recur rs' (conj nodes' beta prod)))))))

(defn index-nodes [nodes]
  (let [sig->id (zipmap (map (juxt :type :id) nodes) (range))
        nodes'  (map (fn [n] (assoc n :id (sig->id ((juxt :type :id) n)))) nodes)
        i-nodes (vec (sort-by :id nodes'))]
    (mapv (fn [node]
            (case (:type node)
              :alpha-test (update node :parent #(get sig->id [:alpha-mem %]))
              :alpha-mem  (update node :parent #(get sig->id [:alpha-test %]))
              :beta-mem   (update node :parent #(get sig->id [:join %]))
              :join       (-> node
                              (update :alpha #(get sig->id [:alpha-mem %]))
                              (update :beta  #(get sig->id [:beta-mem %])))
              :production (update node :parent #(get sig->id [:beta-mem %]))))
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
                              ps     (mapv #(get-in % [1 1]) tests)
                              key-fn `(fn [w#]
                                        (list* :alpha-mem ~id (map #(get w# %) ~ps)))]
                          (assoc n :key-fn key-fn))
            :beta-mem   (let [id     (:id n)
                              tests  (:tests n)
                              ps     (mapv #(get % 2) tests)
                              key-fn `(fn [t#]
                                        (list* :beta-mem ~id (map #(get-in t# %) ~ps)))]
                          (assoc n :key-fn key-fn))
            :join       (let [id           (:id n)
                              tests        (:tests n)
                              alpha-id     (:alpha n)
                              beta-id      (:beta n)
                              ps1          (mapv #(get % 2) tests)
                              alpha-key-fn `(fn [t#]
                                              (list* :alpha-mem ~alpha-id (map #(get-in t# %) ~ps1)))
                              ps2          (mapv #(get-in % [1 1]) tests)
                              beta-key-fn  `(fn [w#]
                                              (list* :beta-mem ~beta-id (map #(get w# %) ~ps2)))]
                          (assoc n :alpha-key-fn alpha-key-fn :beta-key-fn beta-key-fn))
            :production n))
        nodes))

(defn compile-left-activation [nodes R-sym k ts-sym]
  (let [n (nodes k)]
    (case (:type n)
      :beta-mem
      (let [{:keys [key-fn successors]} n]
        `(let [~R-sym (reduce #(update-in %1 (~key-fn %2) (fnil conj #{}) %2) ~R-sym ~ts-sym)]
           (let [~@(interleave (repeat R-sym) (map #(compile-left-activation nodes R-sym % ts-sym) successors))]
             ~R-sym)))
      :join
      (let [{:keys [alpha-key-fn successors]} n]
        `(let [~ts-sym (for [t# ~ts-sym, w# (get-in ~R-sym (~alpha-key-fn t#))] (conj t# w#))]
           (if (empty? ~ts-sym)
             ~R-sym
             (let [~@(interleave (repeat R-sym) (map #(compile-left-activation nodes R-sym % ts-sym) successors))]
               ~R-sym))))
      :production
      (let [{:keys [id priority]} n]
        `(-> ~R-sym
             (update-in [:matches ~id] into (for [t# ~ts-sym] [:+ t#]))
             (update-in [:activated-productions] conj [~priority ~id]))))))

(defn compile-right-activation [nodes R-sym k w-sym]
  (let [n (nodes k)]
    (case (:type n)
      :alpha-test
      (let [{:keys [test successors]} n
            [op pos val] test]
        `(if (~op (get ~w-sym ~pos) ~val)
           (let [~@(interleave (repeat R-sym) (map #(compile-right-activation nodes R-sym % w-sym) successors))]
             ~R-sym)
           ~R-sym))
      :alpha-mem
      (let [{:keys [key-fn successors]} n]
        `(let [~R-sym (update-in ~R-sym (~key-fn ~w-sym) (fnil conj #{}) ~w-sym)]
           (let [~@(interleave (repeat R-sym) (map #(compile-right-activation nodes R-sym % w-sym) successors))]
             ~R-sym)))
      :join
      (let [{:keys [beta-key-fn successors]} n
            ts (gensym)]
        `(let [~ts (get-in ~R-sym (~beta-key-fn ~w-sym))
               ~ts (for [t# ~ts] (conj t# ~w-sym))]
           (if (empty? ~ts)
             ~R-sym
             (let [~@(interleave (repeat R-sym) (map #(compile-left-activation nodes R-sym % ts) successors))]
               ~R-sym)))))))

(defn compile-left-activation- [nodes R-sym k ts-sym]
  (let [n (nodes k)]
    (case (:type n)
      :beta-mem
      (let [{:keys [key-fn successors]} n]
        `(let [~R-sym (reduce #(update-in %1 (~key-fn %2) (fnil disj #{}) %2) ~R-sym ~ts-sym)]
           (let [~@(interleave (repeat R-sym) (map #(compile-left-activation- nodes R-sym % ts-sym) successors))]
             ~R-sym)))
      :join
      (let [{:keys [alpha-key-fn successors]} n]
        `(let [~ts-sym (for [t# ~ts-sym, w# (get-in ~R-sym (~alpha-key-fn t#))] (conj t# w#))]
           (if (empty? ~ts-sym)
             ~R-sym
             (let [~@(interleave (repeat R-sym) (map #(compile-left-activation- nodes R-sym % ts-sym) successors))]
               ~R-sym))))
      :production
      (let [{:keys [id priority]} n]
        `(-> ~R-sym
             (update-in [:matches ~id] into (for [t# ~ts-sym] [:- t#]))
             (update-in [:activated-productions] conj [~priority ~id]))))))

(defn compile-right-activation- [nodes R-sym k w-sym]
  (let [n (nodes k)]
    (case (:type n)
      :alpha-test
      (let [{:keys [test successors]} n
            [op pos val] test]
        `(if (~op (get ~w-sym ~pos) ~val)
           (let [~@(interleave (repeat R-sym) (map #(compile-right-activation- nodes R-sym % w-sym) successors))]
             ~R-sym)
           ~R-sym))
      :alpha-mem
      (let [{:keys [key-fn successors]} n]
        `(let [~R-sym (update-in ~R-sym (~key-fn ~w-sym) (fnil disj #{}) ~w-sym)]
           (let [~@(interleave (repeat R-sym) (map #(compile-right-activation- nodes R-sym % w-sym) successors))]
             ~R-sym)))
      :join
      (let [{:keys [beta-key-fn successors]} n
            ts (gensym)]
        `(let [~ts (get-in ~R-sym (~beta-key-fn ~w-sym))
               ~ts (for [t# ~ts] (conj t# ~w-sym))]
           (if (empty? ~ts)
             ~R-sym
             (let [~@(interleave (repeat R-sym) (map #(compile-left-activation- nodes R-sym % ts) successors))]
               ~R-sym)))))))

(defmacro compile-rules [& rules]
  (let [nodes    (->> rules
                      (map canonicalize-rule)
                      build-nodes
                      index-nodes
                      add-successor-edges
                      add-key-fns)
        a->k     (->> nodes
                      (filter #(= (:type %) :alpha-test))
                      (filter #(nil? (:parent %)))
                      (reduce (fn [m n]
                                (let [[op pos val] (:test n)]
                                  (if (= pos 0)
                                    (update m val (fnil conj #{}) (:id n))
                                    m)))
                              {}))
        dummy-id (:id (first (filter #(= (:flag %) :dummy) nodes)))
        R-sym    (gensym)
        w-sym    (gensym)
        add-wme  `(fn [~R-sym ~w-sym]
                    (case (first ~w-sym)
                      ~@(apply concat
                               (for [[k ids] a->k]
                                 [k `(let [~@(interleave (repeat R-sym)
                                                         (for [id ids, succ (:successors (nodes id))]
                                                           (compile-right-activation nodes R-sym succ w-sym)))]
                                       ~R-sym)]))
                      ~R-sym))
        rem-wme  `(fn [~R-sym ~w-sym]
                    (case (first ~w-sym)
                      ~@(apply concat
                               (for [[k ids] a->k]
                                 [k `(let [~@(interleave (repeat R-sym)
                                                         (for [id ids, succ (:successors (nodes id))]
                                                           (compile-right-activation- nodes R-sym succ w-sym)))]
                                       ~R-sym)]))
                      ~R-sym))]
    {:alpha-mem   {}
     :beta-mem    {dummy-id #{[]}}
     :matches     (->> nodes
                       (filter #(= (:type %) :production))
                       (map (juxt :id (constantly [])))
                       (into {}))
     :productions (->> nodes
                       (filter #(= (:type %) :production))
                       (map (juxt :id :fn))
                       (into {}))
     :activated-productions `(sorted-set-by
                              (fn [x# y#]
                                (let [c# (compare (first y#) (first x#))]
                                  (if (= c# 0) (compare x# y#) c#))))
     :add-wme add-wme
     :rem-wme rem-wme}))
