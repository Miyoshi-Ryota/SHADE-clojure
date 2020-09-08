# shade

A Clojure library designed to optimize function by SHADE(Success-History Based Parameter Adaptation for Differential Evolution).
This library's SHADE targets to minimize objective function which takes vector of continuous value then returns number.

SHADE was proposed below a paper.

`Ryoji Tanabe and Alex Fukunaga. "Success-history based parameter adaptation for differential evolution." In 2013 IEEE congress on evolutionary computation, pp. 71-78. IEEE, 2013.`

[PDF of shade paper is here](https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=Success-history+based+parameter+adaptation+for+differential+evolution&btnG=)


As far as I know, SHADE is one of the best optimization algorithm for function optimization which means single objective 
and continuous problems.



## Usage

```clojure
(ns your-ns.core
  (:require [shade.core :as shade]))

(defn rastrigin-function
  [A theta x]
  (let [n (count x)]
    (+ (* A n)
       (->> x
            (map #(- (* %1 %1) (* A (Math/cos (* theta Math/PI %1)))))
            (apply +)))))

(def evaluate-function (partial rastrigin-function 10.0 2.0))

(def population-size 50)
(def max-generation 1000)

;; A dimension of the solution.
(def dimension-of-problem 2)

;; A domain of the evaluate(objective) function.
(def min-search-range -5)
(def max-search-range 5)

(def last-generation-population 
  (shade/run population-size dimension-of-problem min-search-range max-search-range evaluate-function max-generation))

(def best-solution (first (sort-by :shade.core/fitness last-generation-population)))
(println best-solution)
;; #:shade.core{:scaling-factor 0.9085399934900505, :improved-fitness 7.105427357601002E-15, :crossover-rate 0.7407592708749081, :fitness 0.0, :data (-5.490531981246176E-10 -6.889619712813397E-10)}

(def best-solution-fitness (:shade.core/fitness best-solution))
(println best-solution-fitness)
;; 0.0

(def best-solution-data (:shade.core/data best-solution))
(println best-solution-data)
;; (-5.490531981246176E-10 -6.889619712813397E-10)
```

## License
MIT License

Copyright 2020 Ryota Miyoshi

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

