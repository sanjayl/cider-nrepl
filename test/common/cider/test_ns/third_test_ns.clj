(ns cider.test-ns.third-test-ns
  (:require [cider.test-ns.first-test-ns]))

(defn same-name-testing-function
  "Multiple vars with the same name in different ns's. Used to test ns-list-vars-by-name."
  []
  true)

(defn req-caller
  []
  (cider.test-ns.first-test-ns/target))
