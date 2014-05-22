(ns com.pettomato.rete.util)

(defn update
  "Updates the value in map m at k with the function f.

  Like update-in, but for updating a single top-level key.
  Any additional args will be passed to f after the value."
  ([m k f] (assoc m k (f (get m k))))
  ([m k f x1] (assoc m k (f (get m k) x1)))
  ([m k f x1 x2] (assoc m k (f (get m k) x1 x2)))
  ([m k f x1 x2 & xs] (assoc m k (apply f (get m k) x1 x2 xs))))
