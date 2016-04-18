(ns cider.nrepl.middleware.dependencies-test
  (:require [cider.nrepl.test-session :as session]
            [cider.nrepl.middleware.dependencies :as d]
            [clojure.test :refer :all]))

(use-fixtures :each session/session-fixture)

(deftest dependencies-reply-test
  (testing "return all ns's dependencies."
    (let [deps          (:dependencies (d/dependencies-reply {}))
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
          deps          (:dependencies (d/dependencies-reply {:ns ns}))
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

(deftest uniquify-paths-test
  (testing "Uniquify equivalent cyclic paths"
    (let [paths [[:a :b :c :d :e :a] [:c :d :e :a :a :b] [:a :b :e :d :c]]
          reply (d/uniquify-paths paths)]
      (is (= :a reply ))
      ))

  (testing "return a single ns's dependencies."
    (let [ns            'cider.test-ns.first-test-ns
          deps          (:dependencies (d/dependencies-reply {:ns ns}))
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


#_(deftest integration-test
    (let [response   (session/message {:op "dependencies"})]))

#_(deftest error-handling
    (with-redefs [cp/classpath (fn [] (throw (Exception. "cp error")))]
      (let [response   (session/message {:op "classpath"})]
        (is (= (:status response) #{"done" "classpath-error"}))
        (is (.startsWith (:err response) "java.lang.Exception: cp error"))
        (is (= (:ex response) "class java.lang.Exception"))
        (is (:pp-stacktrace response)))))
