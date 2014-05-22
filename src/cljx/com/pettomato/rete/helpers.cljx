(ns com.pettomato.rete.helpers)

(defn var-symbol? [x]
  (and (symbol? x)
       (= (get (str x) 0) \?)))
