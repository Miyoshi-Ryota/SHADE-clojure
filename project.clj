(defproject mrcsce/shade "0.1.0"
  :description "Clojure's implementation of SHADE(Success-History Based Parameter Adaptation for Differential Evolution).
  See also: https://github.com/Miyoshi-Ryota/SHADE-clojure"
  :url "https://github.com/Miyoshi-Ryota/SHADE-clojure"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/mit-license.php"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [mrcsce/normal-distribution "0.1.0"]
                 [mrcsce/cauchy-distribution "0.1.0"]]
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]]
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :repl-options {:init-ns shade.core})
