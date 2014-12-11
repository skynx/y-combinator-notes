(ns y-combinator-notes.core
  (:use clojure.repl))

  ;; From fatvat: Understanding the Y Combinator
; http://www.fatvat.co.uk/2009/04/understanding-y-combinator.html

  ;; Viksit Gaur: Practical applications of the Y combinator in Clojure
; http://www.viksit.com/tags/clojure/practical-applications-y-combinator-clojure/#fn-364-11

  ;; Brian Gordon: Square roots and fixed points
; https://briangordon.github.io/2014/06/sqrts-and-fixed-points.html

;;;;
;; Notes on Y and other fixed-point combinators
;;;;
  ;; First, we will sum a sequence with explicit recursion
  
(defn sum-seq [x]
  (if (empty? x)
    0
    (+ (first x) (sum-seq (rest x)))))

  ;; Now, we would like to express a function which maps one step in
  ;; the summation procedure to the next


(defn sum-seq-fn-gen [func]
  (fn [s]
    (if (empty? s)
      0
      (+ (first s) (func (rest s))))))

  ;; This is exactly the higher order function which maps a step in
  ;; the summation process to the function expressing the next step
  
  ;user> ((sum-seq-fn-gen nil) [])
  ;0

  ;user> ((sum-seq-fn-gen (sum-seq-fn-generator nil)) [9])
  ;9

  ;user> ((sum-seq-fn-gen (sum-seq-fn-gen (sum-seq-fn-gen nil))) [1 9])
  ;10

  ;; However, we would like to facilitate the fixed point iteration of
  ;; this process with the Y combinator; that is, we require a
  ;; function which is the recursive application of sum-seq-fn-gen
  ;; upon itself.

(defn Y [f]
  ((fn [x] (x x))
   (fn [x]
     (f (fn [& args] (apply (x x) args))))))

(defn sum-seq-Y [s]
  ((Y sum-seq-fn-gen) s))
  
  ;:Proposition
  ;; Any (terminating) explicitly recursive function f'

                                        ;(f' [x] (.. (f' ..)))
  ;; can be transformed into a higher order function f

                                        ;(f [g] (fn [x] (.. (g ..))))
  ;; such that
                                        ; (= ((Y f) x) (f' x))
  ;; for all valid inputs x.

  ;; NB: A function of the form `f` above is referred to as "Y ready"
  ;; or "combinator ready"

  ;; In SICP, Heron's method for calculating square roots is defined
  ;; as follows. First, we decide on an error tolerance.


(def epsilon 0.00000000001)

(defn within-tolerance? [tol u v]
  (< (Math/abs (float (- u v))) tol))

  ;; Next, a function `fixed-point` is defined with help from an
  ;; explicitly recursive function `iter`.

(defn fixed-point [f s]
  (letfn [(iter [old new]
            (if (within-tolerance? epsilon old new)
              new
              (iter new (f new))))]
    (iter s (f s))))

  ;; Heron's method is a fixed point calculation which exploits the
  ;; fact that sqrt(a) = a/sqrt(a) (not quite
                                        ;(= (sqrt a) (/ a (sqrt a)))
  ;; ) and utilises a damping strategy that averages consecutive
  ;; approximations to ensure convergence.

(defn average-damp [f]
  (fn [x]
    (#(/ (reduce + %) (count %)) (list (f x) x))))

(defn sqrt [x] (float (fixed-point (average-damp (fn [y] (/ x y))) 1)))

  ;; Can we do that with Y?

  ;; The function `fixed-point` contains an internally defined
  ;; function `iter` that is handling both iteration and the
  ;; termination condition, `within-epsilon`. By employing the
  ;; proposition above, let's write a Y-ready function from `iter`
  ;; that will help us achieve something similar, separating
  ;; termination and allowing Y to perform the iteration.

(comment
  ; with everything bundled together, our function would look like
  (defn fixed-point-Y [f s]
    (letfn [(iter-Y [g]
              (fn [old new]
                (if (within-epsilon? old new)
                  new
                  (g new (f new)))))])
    ((Y iter-Y) s (f s)))
  ; but by factoring out the internal function, we can make the
  ; intention to test convergence of a function `f` more clear. In
  ; addition, we will gain the ability to interpose code between steps
  ; in the fixed point iteration
  )

(defn test-convergence [f g]
  (fn [old new]
    (if (within-tolerance? epsilon old new)
      new
      (g new (f new)))))

(defn fixed-point-Y [f s]
  ((Y (partial test-convergence f)) s (f s)))

(defn sqrt-Y [x]
  (float (fixed-point-Y (average-damp (fn [y] (/ x y))) 1)))

  ;; This version of sqrt is numerically equivalent to the
  ;; implementation of Heron's method in SICP, but using Y in this way
  ;; opens up the possibility to extend the program in ways that the
  ;; original could not be extended.

  ;; To use an example by Viksit Gaur (and Chris Okasaki), we can log
  ;; every internal function call by writing a modified Y:

(defn logging-Y [f]
  ((fn [x]
     (do
       (prn "Logging call: " x)
       (x x)))
   (fn [x]
     (do
       (prn "Logging within second fn: " x)
       (f (fn [& args]
            (do (prn "Logging within third fn: " args)
                (apply (x x) args))))))))

(defn logging-fixed-point [f s]
  ((logging-Y (partial test-convergence f)) s (f s)))

(defn logging-sqrt [x]
  (float (logging-fixed-point (average-damp (fn [y] (/ x y))) 1)))

;; y-combinator-notes.core> (logging-sqrt 2)
;; "Logging call: " #<core$logging_Y$fn__4649 y_combinator_notes.core$logging_Y$fn__4649@6146fc53>
;; "Logging within second fn: " #<core$logging_Y$fn__4649 y_combinator_notes.core$logging_Y$fn__4649@6146fc53>
;; "Logging within third fn: " (3/2 17/12)
;; "Logging within second fn: " #<core$logging_Y$fn__4649 y_combinator_notes.core$logging_Y$fn__4649@6146fc53>
;; "Logging within third fn: " (17/12 577/408)
;; "Logging within second fn: " #<core$logging_Y$fn__4649 y_combinator_notes.core$logging_Y$fn__4649@6146fc53>
;; "Logging within third fn: " (577/408 665857/470832)
;; "Logging within second fn: " #<core$logging_Y$fn__4649 y_combinator_notes.core$logging_Y$fn__4649@6146fc53>
;; "Logging within third fn: " (665857/470832 886731088897/627013566048)
;; "Logging within second fn: " #<core$logging_Y$fn__4649 y_combinator_notes.core$logging_Y$fn__4649@6146fc53>
;; 1.4142135

  ;; That's fun, and it gives some indication of the possibilities for
  ;; adding instrumentation and other utilities to any iteration of a
  ;; combinator ready function. Gaur also provides an example of
  ;; memoization that speeds up what would be a naive method to
  ;; compute the nth Fibonacci number. The self application combinator
  ;; U = λh.(h h) is expressed in Clojure as follows.

(defn U [h]
  (h h))

  ;; We will write a generic combinator of the form U that utilises an
  ;; arbitrary application function, `my-apply`:

(defn UM [my-apply f]
  (letfn [(g [v]
            (fn [args]
              (my-apply (f v) args)))]
    (f g)))

(defn fib-nr [f]
  (fn [n]
    (if (< n 2) 1
        (+ ((f f) (- n 1))
           ((f f) (- n 2))))))

(defn make-memoizer []
  (let [application-cache (atom {})]
    (fn [function & args]
        (if-let [e (find @application-cache args)]
          (val e)
          (let [result (apply function args)]
            (swap! application-cache assoc args result)
            result)))))

  ;; At the REPL, we see the difference that caching makes on
  ;; execution time:

;; y-combinator-notes.core> (time ((U fib-nr) 20))
;; "Elapsed time: 100.006403 msecs"
;; 10946
;; y-combinator-notes.core> (time ((UM (make-memoizer) fib-nr) 20))
;; "Elapsed time: 1.019526 msecs"
;; 10946

  ;; Of course, this comes at no cost to the code that defines fib-nr.

  ;; The macro below facilitates demonstrating this speed advantage.

(defmacro time-float
  "Evaluates expr and returns the time in nanoseconds it took as a double."
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (float (- (. System (nanoTime)) start#)) ))
