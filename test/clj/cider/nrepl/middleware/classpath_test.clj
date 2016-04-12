(ns cider.nrepl.middleware.classpath-test
  (:require [cider.nrepl.test-session :as session]
            [cider.nrepl.middleware.classpath :as cp]
            [clojure.test :refer :all]))

(deftest dependencies-reply-test
  (let [deps         (cp/dependencies-reply {})
        test-ns-deps (get deps 'cider.test-ns.first-test-ns)]
    (is (map? deps))
    (is (every? set? (vals deps)))
    (is (contains? test-ns-deps 'clojure.set))
    (is (contains? test-ns-deps 'cider.test-ns.second-test-ns))
    (is (not (contains? test-ns-deps 'clojure.walk)))
    (is (not (contains? test-ns-deps 'cider.test-ns.first-test-ns)))
    (is (not (contains? test-ns-deps 'cider.test-ns.third-test-ns)))))

(use-fixtures :each session/session-fixture)
(deftest integration-test
  (let [response   (session/message {:op "classpath"})
        classpaths (:classpath response)]
    (is (= (:status response) #{"done"}))
    (is (> (count classpaths) 1))
    (is (every? string? classpaths))
    (is (some #(re-find #".*clojure-.*jar" %) classpaths))))

(deftest error-handling
  (with-redefs [cp/classpath (fn [] (throw (Exception. "cp error")))]
    (let [response   (session/message {:op "classpath"})]
      (is (= (:status response) #{"done" "classpath-error"}))
      (is (.startsWith (:err response) "java.lang.Exception: cp error"))
      (is (= (:ex response) "class java.lang.Exception"))
      (is (:pp-stacktrace response)))))
