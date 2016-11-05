(ns pettomato.rete.util)

(defn memoize-once
  "Like memoize, but forgets cached mappings after they are used
  once. This is an optimization used to avoid a redundant computation
  when removing a match."
  ;; Used in rete-macros.
  [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (do (swap! mem dissoc args)
            (val e))
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

(defn- inv [op] (case op :+ :- :- :+))

(defn- inv-term [[op v]] [(inv op) v])

(defn invert-signed-terms [terms]
  "Takes a seq of signed terms and returns a new sequence that is the
   the inversion of the original. All signs will be flipped and the
   order of terms will be reversed."
  (map inv-term terms))

(defn collapse-terms
  "Takes a seq of signed terms and returns a new seq of signed terms
  that is the same as the original with complementary pairs (i.e.,
  same value, but opposite polarity) removed.

  Note that this does not preserve the original order of terms."
  [terms]
  (loop [open   terms
         closed #{}]
    (if (empty? open)
      (seq closed)
      (let [term (first open)
            opp  (inv-term term)]
        (if (contains? closed opp)
          (recur (rest open)
                 (disj closed opp))
          (recur (rest open)
                 (conj closed term)))))))
