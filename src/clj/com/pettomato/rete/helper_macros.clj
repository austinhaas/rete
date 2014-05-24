(ns com.pettomato.rete.helper-macros
  (:require
   [clojure.walk :refer [postwalk]]
   [com.pettomato.rete.helpers :refer [var-symbol? default-inv-match memoize-once]]))

(defn build-smap [var-lookup match]
  (reduce-kv #(assoc %1 %2 (get-in match %3)) {} var-lookup))

(defn expr? [x] (and (list? x) (symbol? (first x))))

(defmacro action->rule [schema cache? inv-match-fn]
  (let [{:keys [label preconditions achieves deletes priority]} schema
        ;; 'var->pos' is a mapping from each variable in preconditions to
        ;; the position where it first appears.
        ;;   e.g., [[?a 'x] [?b 'y]] => {?a [0 0], ?b [1 0]}.
        var->pos  (->> preconditions
                       (map-indexed (fn [ri c] (map-indexed (fn [ci t] [t [ri ci]]) c)))
                       (apply concat)
                       (filter (comp var-symbol? first))
                       reverse    ;; So that earlier bindings will replace later bindings.
                       (into {}))
        inv-match-fn (if (true? inv-match-fn)
                       `default-inv-match
                       inv-match-fn)
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
        bindings (distinct @bindings)
        add-fn (gensym)
        rem-fn (gensym)]
    {:conditions `'~preconditions
     :priority   priority
     :fn         `(let [~add-fn (fn [~match]
                                  (let [~@bindings]
                                    ~template))
                        ~add-fn ~(if cache? `(memoize-once ~add-fn) add-fn)
                        ~rem-fn ~(if inv-match-fn
                                   `(comp ~inv-match-fn ~add-fn)
                                   `(constantly []))]
                    (fn ~label [matches#]
                      (reduce (fn [v# [op# match#]]
                                (case op#
                                  :+ (into v# (~add-fn match#))
                                  :- (into v# (~rem-fn match#))))
                              []
                              matches#)))}))

(defmacro defaction
  [name schema & options]
  (let [opts (apply hash-map options)]
    `(def ~name
       (action->rule ~(assoc schema :label name)
                     ~(get opts :cache? false)
                     ~(get opts :inverse-match-fn `default-inv-match)))))
