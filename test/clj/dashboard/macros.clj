(ns dashboard.macros
  (:require [clojure.java.io :as io]))

(defmacro read-json
  "read json from classpath, useful for test resources"
  [f]
  `(js/JSON.parse ~(slurp f)))

(defn slurp-resource [f]
  (slurp (io/resource f)))

(defmacro read-json-resource
  "read json from resources directory, useful for test resources"
  [f]
  `(js/JSON.parse ~(slurp-resource f)))
