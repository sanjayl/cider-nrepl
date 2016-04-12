(ns cider.test-ns.first-test-ns
  "Used for testing the ns and classpath/dependencies middleware"
  (:require [clojure.set :as set]
            [clojure.pprint :as pp]
            [cider.test-ns.second-test-ns :as second]))

(defn same-name-testing-function
  "Multiple vars with the same name in different ns's. Used to test ns-list-vars-by-name."
  []
  true)
