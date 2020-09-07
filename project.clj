(defproject shade "mrcsce/0.1.0-SNAPSHOT"
  :description "Clojure's implementation of SHADE(Success-History Based Parameter Adaptation for Differential Evolution)"
  :url "https://github.com/Miyoshi-Ryota/SHADE-clojure"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/mit-license.php"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [mrcsce/normal-distribution "0.1.0"]]
  :repl-options {:init-ns shade.core})
