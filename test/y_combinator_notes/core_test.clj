(ns y-combinator-notes.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct]
            [y-combinator-notes.core :refer :all]))

(ct/defspec
  Y-combinator-summation-equivalent-to-explicit-recursion
  1000
  (prop/for-all [v (gen/vector gen/int)]
               (= (sum-seq v)
                  ((Y sum-seq-fn-gen) v))))

(ct/defspec
  Y-combinator-sqrt-equivalent-to-Heron-method-in-SICP-on-integers
  1000
  (prop/for-all [v gen/pos-int]
               (= (sqrt v)
                  (sqrt-Y v))))

(ct/defspec
  UM-memoized-fibonacci-faster-than-purely-recursive
  20
  (prop/for-all [v (gen/fmap #(+ 20 %) gen/pos-int)]
                (> (time-float ((U fib-nr) v))
                   (time-float ((UM (make-memoizer) fib-nr) v)))))
