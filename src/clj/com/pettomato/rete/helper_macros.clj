(ns com.pettomato.rete.helper-macros
  (:require
   [clojure.walk :refer [postwalk]]
   [com.pettomato.rete.helpers :refer [var-symbol?]]))

(defn build-smap [var-lookup match]
  (reduce-kv #(assoc %1 %2 (get-in match %3)) {} var-lookup))

(defn expr? [x] (and (list? x) (symbol? (first x))))

(defmacro action->rule [schema remove-matches?]
  (let [{:keys [label preconditions achieves deletes priority]} schema
        add-template (concat (for [x deletes]  [:- x])
                             (for [x achieves] [:+ x]))
        rem-template (concat (for [x achieves] [:- x])
                             (for [x deletes]  [:+ x]))
        match (gensym)
        ;; 'var->pos' is a mapping from each variable in preconditions to
        ;; the position where it first appears.
        ;;   e.g., [[?a 'x] [?b 'y]] => {?a [0 0], ?b [1 0]}.
        var->pos  (->> preconditions
                       (map-indexed (fn [ri c] (map-indexed (fn [ci t] [t [ri ci]]) c)))
                       (apply concat)
                       (filter (comp var-symbol? first))
                       reverse    ;; So that earlier bindings will replace later bindings.
                       (into {}))
        bindings  (atom [])
        expr->var (atom {})
        add-template' (vec (postwalk #(cond
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
                                       :else                  %)
                                     add-template))
        rem-template' (vec (postwalk #(cond
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
                                       :else                  %)
                                     rem-template))
        bindings (distinct @bindings)]
    {:conditions `'~preconditions
     :priority   priority
     :fn         (if remove-matches?
                   `(fn ~label [matches#]
                      (persistent!
                       (reduce (fn [v# [op# ~match]]
                                 (let [~@bindings]
                                   (case op#
                                     :+ (-> v#
                                            ~@(for [x add-template'] `(conj! ~x)))
                                     :- (-> v#
                                            ~@(for [x rem-template'] `(conj! ~x))))))
                               (transient [])
                               matches#)))
                   `(fn ~label [matches#]
                      (persistent!
                       (reduce (fn [v# [op# ~match]]
                                 (let [~@bindings]
                                   (case op#
                                     :+ (-> v#
                                            ~@(for [x add-template'] `(conj! ~x)))
                                     :- v#)))
                               (transient [])
                               matches#))))}))

(defmacro defaction
  [name schema & options]
  (let [opts (apply hash-map options)]
    `(def ~name
       (action->rule ~(assoc schema :label name) ~(get opts :remove-matches? true)))))
