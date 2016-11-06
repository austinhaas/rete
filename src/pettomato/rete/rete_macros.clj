(ns pettomato.rete.rete-macros
  (:require
   [clojure.walk :refer [postwalk]]
   [pettomato.rete.util :refer [memoize-once collapse-terms invert-signed-terms]]))

(defn- var-symbol? [x] (and (symbol? x) (= (get (str x) 0) \?)))

(defn- expr? [x] (and (list? x) (symbol? (first x))))

(defn- synthesize-production [preconds achieves deletes cache? inv-match]
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
                    `invert-signed-terms
                    inv-match)
        add       (gensym)
        rem       (gensym)]
    `(let [~add (fn [~match]
                  (let [~@bindings]
                    ~template))
           ~add ~(if cache? `(memoize-once ~add) add)
           ~rem ~(if inv-match
                   `(comp ~inv-match ~add)
                   `(constantly []))]
       (fn [matches#]
         (reduce (fn [v# [op# match#]]
                   (case op#
                     :+ (into v# (~add match#))
                     :- (into v# (~rem match#))))
                 []
                 matches#)))))

(defn- canonicalize-rule [r]
  (let [r'  (if (contains? r :fn)
              (dissoc r :achieves :deletes :cache? :inv-match)
              (let [{:keys [preconds achieves deletes cache? inv-match]} r]
                (-> r
                    (assoc :fn (synthesize-production preconds achieves deletes cache? inv-match))
                    (dissoc :achieves :deletes :cache? :inv-match))))
        r'' (if (get r' :collapse-matches? false)
              (update-in r' [:fn] #(list `comp % `collapse-terms))
              r')]
    r''))

(defn- condition->alpha-tests [c]
  (->> (map-indexed #(vector '= %1 %2) c)
       (remove (comp var-symbol? peek))))

(defn- condition->equality-tests [ri c vars]
  (->> (map-indexed #(vector [ri %1] %2) c)
       (filter (comp var-symbol? second))
       (map (fn [[pos v]] (vector '= pos (get vars v))))
       (remove (fn [[op p1 p2]] (= p1 p2)))))

(defn- canonicalize-tests [r]
  (let [var->pos    (->> r
                         (map-indexed (fn [ri c] (map-indexed (fn [ci v] [v [ri ci]]) c)))
                         (apply concat)
                         (filter (comp var-symbol? first))
                         reverse
                         (into {}))
        alpha-tests (map condition->alpha-tests r)
        equal-tests (map-indexed #(condition->equality-tests %1 %2 var->pos) r)]
    (map vector alpha-tests equal-tests)))

(defn- build-nodes [rs]
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

(defn- index-nodes [nodes]
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

(defn- add-successor-edges [nodes]
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

(defn- add-key-fns [nodes]
  (mapv (fn [n]
          (case (:type n)
            :alpha-test n
            :alpha-mem  (let [id     (:id n)
                              tests  (:tests n)
                              ps     (mapv #(get-in % [1 1]) tests)
                              w      (gensym)
                              key-fn `(fn [~w] [~id ~@(for [p ps] `(get ~w ~p))])]
                          (assoc n :key-fn key-fn))
            :beta-mem   (let [id     (:id n)
                              tests  (:tests n)
                              ps     (mapv #(get % 2) tests)
                              t      (gensym)
                              key-fn `(fn [~t] [~id ~@(for [p ps] `(get-in ~t ~p))])]
                          (assoc n :key-fn key-fn))
            :join       (let [id           (:id n)
                              tests        (:tests n)
                              alpha-id     (:alpha n)
                              beta-id      (:beta n)
                              ps1          (mapv #(get % 2) tests)
                              t            (gensym)
                              alpha-key-fn `(fn [~t] [~alpha-id ~@(for [p ps1] `(get-in ~t ~p))])
                              ps2          (mapv #(get-in % [1 1]) tests)
                              w            (gensym)
                              beta-key-fn  `(fn [~w] [~beta-id ~@(for [p ps2] `(get ~w ~p))])]
                          (assoc n :alpha-key-fn alpha-key-fn :beta-key-fn beta-key-fn))
            :production n))
        nodes))

(defn- compile-left-activation [nodes R-sym k ts-sym]
  (let [n (nodes k)]
    (case (:type n)
      :beta-mem
      (let [{:keys [key-fn successors]} n]
        `(let [~R-sym (assoc ~R-sym :beta-mem (reduce #(update-in %1 (~key-fn %2) (fnil conj #{}) %2) (:beta-mem ~R-sym) ~ts-sym))]
           (let [~@(interleave (repeat R-sym) (map #(compile-left-activation nodes R-sym % ts-sym) successors))]
             ~R-sym)))
      :join
      (let [{:keys [alpha-key-fn successors]} n]
        `(let [~ts-sym (reduce (fn [acc# t#]
                                 (reduce (fn [acc# w#] (conj acc# (conj t# w#)))
                                         acc#
                                         (get-in (:alpha-mem ~R-sym) (~alpha-key-fn t#))))
                               []
                               ~ts-sym)]
           (if (empty? ~ts-sym)
             ~R-sym
             (let [~@(interleave (repeat R-sym) (map #(compile-left-activation nodes R-sym % ts-sym) successors))]
               ~R-sym))))
      :production
      (let [{:keys [id priority]} n]
        `(-> ~R-sym
             (update-in [:matches ~id] into (for [t# ~ts-sym] [:+ t#]))
             (update-in [:activated-productions] conj [~priority ~id]))))))

(defn- compile-right-activation [nodes R-sym k w-sym]
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
        `(let [~R-sym (assoc ~R-sym :alpha-mem (update-in (:alpha-mem ~R-sym) (~key-fn ~w-sym) (fnil conj #{}) ~w-sym))]
           (let [~@(interleave (repeat R-sym) (map #(compile-right-activation nodes R-sym % w-sym) successors))]
             ~R-sym)))
      :join
      (let [{:keys [beta-key-fn successors]} n
            ts (gensym)]
        `(let [~ts (get-in (:beta-mem ~R-sym) (~beta-key-fn ~w-sym))
               ~ts (mapv (fn [t#] (conj t# ~w-sym)) ~ts)]
           (if (empty? ~ts)
             ~R-sym
             (let [~@(interleave (repeat R-sym) (map #(compile-left-activation nodes R-sym % ts) successors))]
               ~R-sym)))))))

(defn- compile-left-activation- [nodes R-sym k ts-sym]
  (let [n (nodes k)]
    (case (:type n)
      :beta-mem
      (let [{:keys [key-fn successors]} n]
        `(let [~R-sym (assoc ~R-sym :beta-mem (reduce #(update-in %1 (~key-fn %2) (fnil disj #{}) %2) (:beta-mem ~R-sym) ~ts-sym))]
           (let [~@(interleave (repeat R-sym) (map #(compile-left-activation- nodes R-sym % ts-sym) successors))]
             ~R-sym)))
      :join
      (let [{:keys [alpha-key-fn successors]} n]
        `(let [~ts-sym (reduce (fn [acc# t#]
                                 (reduce (fn [acc# w#] (conj acc# (conj t# w#)))
                                         acc#
                                         (get-in (:alpha-mem ~R-sym) (~alpha-key-fn t#))))
                               []
                               ~ts-sym)]
           (if (empty? ~ts-sym)
             ~R-sym
             (let [~@(interleave (repeat R-sym) (map #(compile-left-activation- nodes R-sym % ts-sym) successors))]
               ~R-sym))))
      :production
      (let [{:keys [id priority]} n]
        `(-> ~R-sym
             (update-in [:matches ~id] into (for [t# ~ts-sym] [:- t#]))
             (update-in [:activated-productions] conj [~priority ~id]))))))

(defn- compile-right-activation- [nodes R-sym k w-sym]
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
        `(let [~R-sym (assoc ~R-sym :alpha-mem (update-in (:alpha-mem ~R-sym) (~key-fn ~w-sym) (fnil disj #{}) ~w-sym))]
           (let [~@(interleave (repeat R-sym) (map #(compile-right-activation- nodes R-sym % w-sym) successors))]
             ~R-sym)))
      :join
      (let [{:keys [beta-key-fn successors]} n
            ts (gensym)]
        `(let [~ts (get-in (:beta-mem ~R-sym) (~beta-key-fn ~w-sym))
               ~ts (mapv (fn [t#] (conj t# ~w-sym)) ~ts)]
           (if (empty? ~ts)
             ~R-sym
             (let [~@(interleave (repeat R-sym) (map #(compile-left-activation- nodes R-sym % ts) successors))]
               ~R-sym)))))))

(defmacro compile-rules
  "Compiles the supplied rules into a rete network.

  Each rule is a map with the following fields:

    :preconds : A seq of [tag & vals].

    :achieves : (Optional) A seq of [tag & vals] pairs. Default: [].

    :deletes  : (Optional) A seq of [tag & vals] pairs. Default: [].

    :inv-match : (Optional) True, a fn, or nil. Default: nil.

      This field implements truth maintenance for the production. This
      comes into play when some fact that previously caused this
      production to fire has been retracted. This production may have
      generated new facts based on that fact that should now also be retracted.

      If true, the default fn will be used, which simply uses the
      original production function to generate the result again, but flips
      the signs of the result values.

      If a fn, that function should take the match and return the
      values that should be added or removed. This functions is
      effectively the opposite of the normal production function.

      If not provided, no action will be taken when a relevant fact is
      retracted.

    :collapse-matches? : (Optional) Boolean. Default: false.

      If true, then any pending matches that have the same value, but
      opposite polarity will cancel out. If false, or not supplied, then
      both values will trigger the rule.

    :priority : (Optional) Any value that can be compared using compare.

      Rules are fired, first, in order of any supplied priority
      values, with higher values indicating higher priority, and second,
      in the order they were supplied.

    :cache? : (Optional) Boolean. Default: false.

      If true, then the rule will be memoized with a special
      memoization function that only remembers a result once. In other
      words, the second time the production is called with the same input,
      it'll return the cached value, but then immediately forget it. This
      is an optimization to avoid repeating an expensive calculation when
      removing a match.

    :fn : (Optional) A function that takes a seq of signed matches and
          returns a seq of signed facts.

      This is the longhand version used to specify a production
      function that cannot be expressed using the above fields. This is
      needed when the set of output fact types isn't static.

  tag: Any value that uniquely identifies the type of a fact.

  val: A value or a variable, which is a symbol that begins with a '?',
       such as ?a.
  "
  [& rules]
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
