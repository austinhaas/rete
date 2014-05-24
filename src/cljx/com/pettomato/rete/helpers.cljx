(ns com.pettomato.rete.helpers)

(defn var-symbol? [x]
  (and (symbol? x)
       (= (get (str x) 0) \?)))

(defn default-inv-match [xs]
  ;; Flip ops. Reverse the whole thing by using a list.
  (reduce (fn [acc [op v]] (cons (case op :- [:+ v] :+ [:- v]) acc))
          ()
          xs))

(defn memoize-once
  [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (do (swap! mem dissoc args)
            (val e))
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))
