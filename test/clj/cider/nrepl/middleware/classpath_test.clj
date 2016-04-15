(ns cider.nrepl.middleware.classpath-test
  (:require [cider.nrepl.test-session :as session]
            [cider.nrepl.middleware.classpath :as cp]
            [clojure.test :refer :all]))

(deftest dependencies-reply-test
  (testing "return all ns's dependencies."
    (let [deps          (:dependencies (cp/dependencies-reply {}))
          test-ns-reply (filter (fn [[n d]] (= n 'cider.test-ns.first-test-ns)) deps)
          test-ns-deps  (->> test-ns-reply first rest (apply set))]
      (is (> (count deps) 3))
      (is (seq? deps))
      (is (every? vector? deps))
      (is (test-ns-deps 'clojure.set))
      (is (test-ns-deps 'cider.test-ns.second-test-ns))
      (is (not (test-ns-deps 'clojure.walk)))
      (is (not (test-ns-deps 'cider.test-ns.first-test-ns)))
      (is (not (test-ns-deps 'cider.test-ns.third-test-ns)))))

  (testing "return a single ns's dependencies."
    (let [ns            'cider.test-ns.first-test-ns
          deps          (:dependencies (cp/dependencies-reply {:ns ns}))
          test-ns-reply (filter (fn [[n d]] (= n ns)) deps)
          test-ns-deps  (->> test-ns-reply first rest (apply set))]
      (is (= (count deps) 1))
      (is (seq? deps))
      (is (every? vector? deps))
      (is (test-ns-deps 'clojure.set))
      (is (test-ns-deps 'cider.test-ns.second-test-ns))
      (is (not (test-ns-deps 'clojure.walk)))
      (is (not (test-ns-deps 'cider.test-ns.first-test-ns)))
      (is (not (test-ns-deps 'cider.test-ns.third-test-ns))))))

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
