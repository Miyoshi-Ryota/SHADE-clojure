(ns shade.core
  (:require [clojure.spec.alpha :as s]))

"ToDo:
* muationの足し算などをベクトル演算できるようにする
* 子をfitnessがいい場合のみ置き換えるようにする
* SHADEのパラメータ調整を実装する
* もしかしたら、solutionをmapにしてgenとfitnessを持つようにした方がいいかも"

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

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
(s/def ::solution (s/keys :req [::data ::fitness]))

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
              ::fitness (evaluate-function solution-data))))

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
  (let [crossed-over-data (map #(crossover-one-gene %1 %2 crossover-rate) (::data parent-sol) (::data child-sol))]
    (hash-map ::data crossed-over-data
              ::fitness (evaluate-function crossed-over-data))))

(defn- get-pbest-solution
  [p population]
  {:pre [(s/valid? double p)
         (s/valid? ::population population)]
   :post [(s/valid? ::solution %)]}
  (->> population
       (sort-by ::fitness)
       (take (int (* (count population) p)))
       (rand-nth)))

(defn- rastrigin-function
  [A theta x]
  {:pre [(s/valid? (s/coll-of number?) x)
         (s/valid? double? A)
         (s/valid? double? theta)]
   :post [(s/valid? (s/and double?) %)]}
  (let [n (count x)]
    (+ (* A n)
       (->> x
            (map #(- (* %1 %1) (* A (Math/cos (* theta Math/PI %1)))))
            (apply +)))))

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
              ::fitness nil)))

(defn- select-better-solution
  [solution1 solution2]
  {:pre [(s/valid? ::solution solution1)
         (s/valid? ::solution solution2)]
   :post [(s/valid? ::solution %)]}
  (if (< (::fitness solution1) (::fitness solution2)) solution1
                                                      solution2))

(defn- select-next-population
  [now-pop
   crossed-over-pop]
  {:pre [(s/valid? ::population now-pop)
         (s/valid? ::population crossed-over-pop)]
   :post [(s/valid? ::population %)]}
  (map select-better-solution now-pop crossed-over-pop))

(defn run
  [population-size dimension min_x max_x evaluate-function max_generation]
  (loop [population (initialize-population population-size dimension min_x max_x evaluate-function)
         generation 1]
    (if (<= max_generation generation)
      population
      (let [children (->> population
                          (map #(mutation % 0.5 population))
                          (map #(crossover %1 %2 0.5 evaluate-function) population))]
        (recur (select-next-population population children) (inc generation))))))

(run 50 2 -5 5 (partial rastrigin-function 10.0 2.0) 1000)
