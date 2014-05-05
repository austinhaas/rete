(ns com.pettomato.rete.helpers
  (:require
   [clojure.walk :refer (postwalk)]
   [com.pettomato.rete :refer [var-symbol? add-wme remove-wme get-matches clear-matches]]))

(defn build-smap [var-lookup match]
  (reduce-kv #(assoc %1 %2 (get-in match %3)) {} var-lookup))

(defn action->rule [action-schema remove-matches?]
  (let [{:keys [preconditions achieves deletes]} action-schema
        vars (->> preconditions
                  (map-indexed (fn [ri c] (map-indexed (fn [ci t] [t [ri ci]]) c)))
                  (apply concat)
                  (filter (comp var-symbol? first))
                  reverse
                  (into {}))
        add-template (concat (for [x deletes]  ['- x])
                             (for [x achieves] ['+ x]))
        rem-template (concat (for [x achieves] ['- x])
                             (for [x deletes]  ['+ x]))]
    {:conditions  preconditions
     :add-matches (fn [ms] (mapcat (fn [match]
                                     (let [smap (build-smap vars match)]
                                       (postwalk
                                        #(cond
                                          (contains? smap %)              (get smap %)
                                          (and (coll? %) (fn? (first %))) (apply (first %) (rest %))
                                          :else                           %)
                                        add-template)))
                                   ms))
     :rem-matches (if remove-matches?
                    (fn [ms] (mapcat (fn [match]
                                       (let [smap (build-smap vars match)]
                                         (postwalk
                                          #(cond
                                            (contains? smap %)              (get smap %)
                                            (and (coll? %) (fn? (first %))) (apply (first %) (rest %))
                                            :else                           %)
                                          rem-template)))
                                     ms))
                    (constantly []))}))


{:preconditions [[:a '?a] [:b '?b]]
 :achieves      [[:c (list + (list * '?a '?a) '?b)]]}

=>

(let [a (get-in match [0 1])
      b (get-in match [1 1])
      x (* a a)
      y (+ x b)]
  [:c y])

(defn expr? [x] (and (coll? x) (fn? (first x))))

(defn parse-achieves [schema]
  (let [{:keys [preconditions achieves]} schema
        vars (->> preconditions
                  (map-indexed (fn [ri c] (map-indexed (fn [ci t] [t [ri ci]]) c)))
                  (apply concat)
                  (filter (comp var-symbol? first))
                  reverse
                  (into {}))
        bindings (atom [])]
    (postwalk #(cond
                (contains? vars %) (let [v %] (swap! bindings conj [v (list get-in 'matches (get vars %))]) v)
                (expr? %)          (let [v (gensym)] (swap! bindings conj [v %]) v)
                :else              %)
              achieves)
    (distinct @bindings)))

(parse-achieves
 {:preconditions [[:a '?a] [:b '?b]]
  :achieves      [[:c (list + (list * '?a '?a) '?b)]]})


(defn add-until-stable
  ;; dfs
  ([R ops] (add-until-stable R ops 100))
  ([R ops max-iterations]
     (loop [R R
            open ops
            all-matches []
            safety 0]
       (assert (< safety max-iterations))
       (if (empty? open)
         [R all-matches]
         (let [[op w]  (first open)
               R'      (case op
                         + (add-wme R w)
                         - (remove-wme R w))
               matches (get-matches R')]
           (if (empty? matches)
             (recur R' (rest open) all-matches (inc safety))
             (let [ms (reduce into [] matches)]
               (recur (clear-matches R')
                      (into ms (rest open))
                      (into all-matches ms)
                      (inc safety)))))))))
