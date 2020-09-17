(ns shade.core-test
  (:require [clojure.test :as t]
            [shade.core :as shade]
            [clojure.spec.alpha :as s]))

(defn- rastrigin-function
  [A theta x]
  {:pre  [(s/valid? (s/coll-of number?) x)
          (s/valid? double? A)
          (s/valid? double? theta)]
   :post [(s/valid? (s/and double?) %)]}
  (let [n (count x)]
    (+ (* A n)
       (->> x
            (map #(- (* %1 %1) (* A (Math/cos (* theta Math/PI %1)))))
            (apply +)))))

(t/deftest can-optimize-rastrigin-test
  (t/testing "Test of that SHADE can optimize rastrigin function."
    (let [last-population (shade/run 50 2 -5 5 (partial rastrigin-function 10.0 2.0) 1000)]
      (t/is (= 0.0 (:shade.core/fitness (first (sort-by :shade.core/fitness last-population))))))))

(t/deftest can-optimize-high-dimension-rastrigin-test
  (t/testing "Test of that SHADE can optimize high dimension rastrigin function."
    (let [last-population (shade/run 50 15 -5 5 (partial rastrigin-function 10.0 2.0) 1000)]
      (t/is (= 0.0 (:shade.core/fitness (first (sort-by :shade.core/fitness last-population))))))))
