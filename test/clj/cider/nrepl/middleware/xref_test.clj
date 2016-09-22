(ns cider.nrepl.middleware.xref-test
  (:require [cider.nrepl.middleware.xref :as xref])
  (:use [clojure.test]))


(deftest fn-uses-var?-tests
  (testing "require as"
    (is (#'xref/can-ns-resolve-var? (find-ns 'cider.test-ns.second-test-ns)
                                    (resolve (symbol 'cider.test-ns.first-test-ns/target)))))

  (testing "require"
    (is (#'xref/can-ns-resolve-var? (find-ns 'cider.test-ns.third-test-ns)
                                    (resolve (symbol 'cider.test-ns.first-test-ns/target)))))

  (testing "use"
    (is (#'xref/can-ns-resolve-var? (find-ns 'cider.test-ns.fourth-test-ns)
                                    (resolve (symbol 'cider.test-ns.first-test-ns/target))))))

