(ns org.soulspace.clj.math.vector)

(defn addition
  [a b]
  (if (= (count a) (count b))
    (vec (map + a b))
    (throw (IllegalArgumentException. "Vectors are not of the same size."))))

(defn substraction
  [a b]
  (if (= (count a) (count b))
    (vec (map - a b))
    (throw (IllegalArgumentException. "Vectors are not of the same size."))))

(defn scalar-product
  [r a]
  (vec (map #(* r %) a)))

(defn dot-product
  [a b]
  (if (= (count a) (count b))
    (reduce + (map * a b))
    (throw (IllegalArgumentException. "Vectors are not of the same size."))))

(defn cross-product
  [a b]
  (if (= (count a) (count b))
    nil ; TODO calculate cross product
    (throw (IllegalArgumentException. "Vectors are not of the same size."))))
