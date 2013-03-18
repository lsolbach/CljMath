(ns org.soulspace.clj.math.math
  (:use 
    [org.soulspace.clj.math.java-math 
     ;:only [pi e floor abs exp sin]
     ]))

; mathematical functions 

; some functions are in the spirit of 
; 'Structure and Interpretation of Computer Programs'
; by Abelson, Sussman and Sussman

(def epsilon 0.001)
(def dx 0.0000001)

(defn sqr [x]
  "square of x"
  (* x x))

(defn cube [x]
  "cube of x"
  (* x x x))

(defn avg 
  "calculate the avarage of x and y or coll"
  ([x y]
    (/ (+ x y) 2))
  ([coll]
    (/ (reduce + 0 coll) (count coll))))

(defn gcd [x y]
  "calculate the greatest common divisor of x and y"
  (if (= y 0)
    x
    (gcd y (rem x y))))

(defn round-up [x n]
  (/ (floor (+ (* x (exp 10 n)) 0.5)) (exp 10 n )))

(defn close-enough? [x y]
  "check for a difference smaller than epsilon"
  (< (abs (- x y)) epsilon))

(defn average-damp [f]
  (fn [x] (avg x (f x))))


; refactor to loop/recur
(defn sum [term a nxt b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (nxt a) nxt b))))

; refactor to loop/recur
(defn prod [term a nxt b]
  (if (> a b)
    0
    (* (term a)
       (prod term (nxt a) nxt b))))

(defn integral [f a b dx]
  "calculate the integral of f between a and b with dx"
  (defn add-dx [x] (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b) dx))

(defn search [f neg-point pos-point] 
  "search for zero"
  (let [midpoint (avg neg-point pos-point)]
    (if (close-enough? neg-point pos-point)
        midpoint
        (let [test-value (f midpoint)]
          (cond
            (pos? test-value) (recur f neg-point midpoint)
            (neg? test-value) (recur f midpoint pos-point)
            :default midpoint)))))

(defn half-intervall [f a b]
  "half intervall method for function f and values a and b"
  (let [a-value (f a)
        b-value (f b)]
    (cond
      (and (neg? a-value) (pos? b-value)) (search f a b)
      (and (neg? b-value) (pos? a-value)) (search f b a)
      :default (throw (RuntimeException. (str "Values are not of opposite sign: " a " " b))))))

(defn fixed-point [f first-guess]
  "fixed point method"
  (defn try-guess [guess]
    (let [next-guess (f guess)]
      (if (close-enough? guess next-guess)
          next-guess
          (recur next-guess))))
  (try-guess first-guess))

(defn deriv [g]
  "derive function g"
  (fn [x]
    (/ (- (g (+ x dx)) (g x))
       dx)))

(defn newton-transform [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))

(defn newton-method [g guess]
  "newton method for searching the roots of g"
  (fixed-point (newton-transform g) guess))

(defn factorial [x]
  "factorial"
  (loop [curr x fact 1]
    (cond
      (<= curr 0) 0
      (= curr 1) fact
      :default (recur (dec curr) (*' curr fact)))))

(defn fibonacci [x]
  "fibonacci number"
  (loop [a 1 b 0 cnt x]
    (cond
      (= cnt 0) b
      :default (recur (+' a b) a (dec cnt)))))
