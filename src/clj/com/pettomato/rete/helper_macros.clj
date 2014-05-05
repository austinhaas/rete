(ns com.pettomato.rete.helper-macros
  (:require
   [clojure.walk :refer (postwalk)]
   [com.pettomato.rete :refer [var-symbol?]]))

(defn build-smap [var-lookup match]
  (reduce-kv #(assoc %1 %2 (get-in match %3)) {} var-lookup))

(defn expr? [x] (and (list? x) (symbol? (first x))))

(defn get-bindings [preconditions template match-sym]
  ;; 'var->pos' is a mapping from each variable in preconditions to
  ;; the position where it first appears.
  ;;   e.g., [[?a 'x] [?b 'y]] => {?a [0 0], ?b [1 0]}.
  (let [var->pos  (->> preconditions
                       (map-indexed (fn [ri c] (map-indexed (fn [ci t] [t [ri ci]]) c)))
                       (apply concat)
                       (filter (comp var-symbol? first))
                       reverse    ;; So that earlier bindings will overwrite later bindings.
                       (into {}))
        bindings  (atom [])
        expr->var (atom {})
        template' (postwalk #(cond
                              (contains? var->pos %) (let [v    %
                                                           pos  (get var->pos v)
                                                           expr `(get-in ~match-sym ~pos)]
                                                       (swap! bindings into [v expr])
                                                       v)
                              (expr? %)              (let [v    (@expr->var % (gensym))
                                                           expr %]
                                                       (swap! bindings into [v expr])
                                                       (swap! expr->var assoc % v)
                                             v)
                              :else                  %)
                            template)]
    [(distinct @bindings) (vec template')]))

(defmacro compile-effects [preconditions template]
  (let [matches (gensym)
        match (gensym)
        [bindings template'] (get-bindings preconditions template match)]
    `(fn [~matches]
       (mapcat (fn [~match]
                 (let [~@bindings]
                   ~template'))
               ~matches))))

(defmacro action->rule [schema remove-matches?]
  (let [{:keys [preconditions achieves deletes]} schema
        add-template (concat (for [x deletes]  [:- x])
                             (for [x achieves] [:+ x]))
        rem-template (concat (for [x achieves] [:- x])
                             (for [x deletes]  [:+ x]))]
    {:conditions `'~preconditions
     :add-matches `(compile-effects ~preconditions ~add-template)
     :rem-matches (if remove-matches?
                    `(compile-effects ~preconditions ~rem-template)
                    `(constantly []))}))

(defmacro defaction
  [name schema & options]
  (let [opts (apply hash-map options)]
    `(def ~name
       (action->rule ~schema ~(get opts :remove-matches? true)))))
