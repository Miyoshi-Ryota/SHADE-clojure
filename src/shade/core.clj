(ns shade.core
  (:require [clojure.spec.alpha :as s]
            [normal-distribution.core :as n]
            [clojure.set :as set]
            [cauchy-distribution.core :as c]))

(defn- normalize
  "Normalize `target` number to between `translated_min` and `translated_max`"
  [target target_min target_max translated_min translated_max]
  {:pre [(s/valid? (s/and number?
                          #(<= target_min % target_max)) target)
         (s/valid? (s/and number?
                          #(> target_max %)) target_min)
         (s/valid? (s/and number?
                          #(< target_min %)) target_max)
         (s/valid? (s/and number?
                          #(> translated_max %)) translated_min)
         (s/valid? (s/and number?
                          #(< translated_min %)) translated_max)]
   :post [(s/valid? (s/and number?
                          #(<= translated_min % translated_max)) %)]}
  (+ (* (/ (- target target_min)
           (- target_max target_min))
        (- translated_max translated_min))
     translated_min))

(defn- generate-rand-between
  [min max]
  {:pre [(s/valid? (s/and number?
                          #(> max %)) min)
         (s/valid? (s/and number?
                          #(< min %)) max)]
   :post [(s/valid? (s/and number?
                          #(<= min % max)) %)]}
  (-> (rand)
      (normalize 0.0 1.0 min max)))


(s/def ::data (s/coll-of number?))
(s/def ::fitness (s/nilable number?))
(s/def ::crossover-rate (s/nilable number?))
(s/def ::scaling-factor (s/nilable number?))
(s/def ::improved-fitness (s/nilable number?))
(s/def ::solution (s/keys :req [::data ::fitness ::crossover-rate ::scaling-factor ::improved-fitness]))

(s/def ::population (s/coll-of ::solution))

(defn- initialize-solution
  [dimension lower-bound upper-bound evaluate-function]
  {:pre [(s/valid? int? dimension)
         (s/valid? (s/and number?
                          #(< % upper-bound)) lower-bound)
         (s/valid? (s/and number?
                          #(> % lower-bound)) upper-bound)]
   :post [(s/valid? ::solution %)]}
  (let [solution-data (->> (partial generate-rand-between -5 5)
                           (repeatedly dimension))]
    (hash-map ::data solution-data
              ::fitness (evaluate-function solution-data)
              ::crossover-rate nil
              ::scaling-factor nil
              ::improved-fitness nil)))

(defn- initialize-population
  [population-size dimension lower-bound upper-bound evaluate-function]
  {:pre [(s/valid? int? population-size)
         (s/valid? int? dimension)
         (s/valid? (s/and number?
                          #(> upper-bound %)) lower-bound)
         (s/valid? (s/and number?
                          #(< lower-bound %)) upper-bound)]
   :post [(s/valid? ::population %)]}
  (->> (partial initialize-solution dimension lower-bound upper-bound evaluate-function)
       (repeatedly population-size)))


(defn- crossover-one-gene
  [parent-gene child-gene crossover-rate]
  {:pre [(s/valid? number? parent-gene)
         (s/valid? number? child-gene)
         (s/valid? number? crossover-rate)]
   :post [(s/valid? number? %)]}
  (if (> (rand) crossover-rate) parent-gene
                                child-gene))

(defn- crossover
  [parent-sol child-sol crossover-rate evaluate-function]
  {:pre [(s/valid? ::solution parent-sol)
         (s/valid? ::solution child-sol)]
   :post [(s/valid? ::solution %)]}
  (let [crossed-over-data (map #(crossover-one-gene %1 %2 crossover-rate) (::data parent-sol) (::data child-sol))
        fitness (evaluate-function crossed-over-data)]
    (hash-map ::data crossed-over-data
              ::fitness fitness
              ::crossover-rate crossover-rate
              ::scaling-factor (::scaling-factor child-sol)
              ::improved-fitness (- (::fitness parent-sol) fitness))))

(defn- get-pbest-solution
  [p population]
  {:pre [(s/valid? double p)
         (s/valid? ::population population)]
   :post [(s/valid? ::solution %)]}
  (->> population
       (sort-by ::fitness)
       (take (int (* (count population) p)))
       (rand-nth)))


(defn- mutation-one-gene
  [x x_r1 x_r2 x_pbest f]
  (+ x
     (* f (- x_pbest x))
     (* f (- x_r1 x_r2))))

(defn- mutation
  "current-to-pbest-1"
  [solution scaling-factor population]
  (let [x (::data solution)
        x_r1 (::data (rand-nth population))
        x_r2 (::data (rand-nth population))
        x_pbest (::data (get-pbest-solution 0.2 population))
        mutated-data (map #(mutation-one-gene %1 %2 %3 %4 scaling-factor) x x_r1 x_r2 x_pbest)]
    (hash-map ::data mutated-data
              ::fitness nil
              ::scaling-factor scaling-factor
              ::crossover-rate nil
              ::improved-fitness nil)))

(defn- select-better-solution
  [solution1 solution2]
  {:pre [(s/valid? ::solution solution1)
         (s/valid? ::solution solution2)]
   :post [(s/valid? ::solution %)]}
  (if (<= (::fitness solution1) (::fitness solution2)) solution1
                                                      solution2))

(defn- select-next-population
  [now-pop
   crossed-over-pop]
  {:pre [(s/valid? ::population now-pop)
         (s/valid? ::population crossed-over-pop)]
   :post [(s/valid? ::population %)]}
  (map select-better-solution now-pop crossed-over-pop))

(defn- add-gaussian-noise
  [n]
  {:pre [(s/valid? number? n)]
   :post [(s/valid? number? %)]}
  (let [noised-n (n/sample-normal-distribution n 0.1)]
    (cond (>= noised-n 1) 1.0
          (<= noised-n 0) 0.0
          :else noised-n)))

(defn- add-cauchy-noise
  [n]
  {:pre [(s/valid? number? n)]
   :post [(s/valid? number? %)]}
  (let [noised-n (c/sample-cauchy-distribution n 0.1)]
    (cond (>= noised-n 1) 1.0
          (<= noised-n 0) (add-cauchy-noise n)
          :else noised-n)))

(defn- update-memory-crossover-rate
  [memory-crossover-rate
   memory-index
   successful-crossover-rate
   improved-fitness]
  (if (= 0 (count successful-crossover-rate))
    memory-crossover-rate
    (let [sum-of-improved (apply + improved-fitness)
          weight (map #(/ % sum-of-improved) improved-fitness)]
      (->> (map #(* %1 %2) weight successful-crossover-rate)
           (apply +)
           (assoc memory-crossover-rate memory-index)))))

(defn- update-memory-scaling-factor
  [memory-scaling-factor
   memory-index
   successful-scaling-factor
   improved-fitness]
  (if (= 0 (count successful-scaling-factor))
    memory-scaling-factor
    (let [sum-of-improved (apply + improved-fitness)
          weight (map #(/ % sum-of-improved) improved-fitness)]
      (->>
        (/ (->> (map #(Math/pow %1 2) successful-scaling-factor)
                (map #(* %1 %2) weight)
                (apply +))
           (->> (map #(* %1 %2) weight successful-scaling-factor)
                (apply +)))
        (assoc memory-scaling-factor memory-index)))))

(defn run
  "optimize (minimize) `evaluate-function` by SHADE.
  `population-size` is population-size.
  `dimension` is dimension of solution.
  `min_x` is a min domain of the problem (`evaluate-function`).
  `max_x` is a max domain of the problem (`evaluate-function`).
  `evaluate-function` is function which takes continuous values of `dimension` vector then returns number."
  [population-size dimension min_x max_x evaluate-function max_generation]
  (loop [population (initialize-population population-size dimension min_x max_x evaluate-function)
         generation 1
         memory-crossover-rate [0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5]
         memory-scaling-factor [0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5]]
    (if (<= max_generation generation)
      population
      (let [memory-index (mod generation 8)
            crossover-rates (repeatedly (partial add-gaussian-noise (nth memory-crossover-rate memory-index)))
            scaling-factors (repeatedly (partial add-cauchy-noise (nth memory-scaling-factor memory-index)))
            children (->> population
                          (map #(mutation %2 %1 population) scaling-factors)
                          (pmap #(crossover %1 %3 %2 evaluate-function) population crossover-rates))
            next-generation-population (select-next-population population children)
            successful-children (set/difference (set next-generation-population) (set population))
            successful-crossover-rate (map ::crossover-rate successful-children)
            successful-scaling-factor (map ::scaling-factor successful-children)
            improved-fitness (map ::improved-fitness successful-children)
            memory-crossover-rate (update-memory-crossover-rate memory-crossover-rate memory-index successful-crossover-rate improved-fitness)
            memory-scaling-factor (update-memory-scaling-factor memory-scaling-factor memory-index successful-scaling-factor improved-fitness)]
        (recur next-generation-population (inc generation) memory-crossover-rate memory-scaling-factor)))))

