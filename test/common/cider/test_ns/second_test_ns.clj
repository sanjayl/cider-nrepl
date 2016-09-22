(ns cider.test-ns.second-test-ns
  (:require [cider.test-ns.first-test-ns :as f]))

(defn same-name-testing-function
  "Multiple vars with the same name in different ns's. Used to test ns-list-vars-by-name."
  []
  true)

(defn req-as-caller
  "Used for xref testing"
  []
  (f/target))
