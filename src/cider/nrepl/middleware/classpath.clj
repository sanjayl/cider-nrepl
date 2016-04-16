(ns cider.nrepl.middleware.classpath
  (:require [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [clojure.java.classpath :as cp]
            [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
            [clojure.tools.namespace.dir :as dir]
            [clojure.tools.namespace.file :as file]
            [clojure.tools.namespace.track :as track]))

(defn classpath []
  (map str (cp/classpath)))

(defn find-cycles [cur {:keys [seen root stack graph] :as state}]
  (first (filter identity (for [c (remove seen cur)]
                            (if (= c root)
                              (conj stack c)
                              (find-cycles (get graph c) (-> state
                                                             (update-in [:stack] conj c)
                                                             (update-in [:seen] conj c))))))))

(defn cycle-paths [graph]
  (filterv identity
           (for [[root deps] graph
                 :let [stack (find-cycles deps {:seen #{} :stack [root] :graph graph :root root})]]
             stack)))

(defn classpath-reply [msg]
  {:classpath (classpath)})

(defn dependencies-reply
  "If no `ns` is supplied in the `msg`, returns an alphabetically
  sorted list of pairs, with head being a namespace and tail being an
  alphabetically sorted list of head's dependencies. This will cover
  all the namespaces on the classpath.

  If a `ns` is given in the `msg`, then a similar data structure will
  be returned, but just for the ns provided. If `ns` cannot be found,
  then an empty list is returned."
  [{:keys [ns] :as msg}]
  (as-> (dir/scan-all {}) $
    (get-in $ [::track/deps :dependencies]) ;; {ns4 #{ns4-deps}, ns1 #{ns1-deps}, ns2 ... }
    (if ns
      (apply hash-map (find $ (symbol ns))) ;; if looking for just ns* -> filter: {ns* #{ns*-deps}}
      $)                                    ;; otherwise keep all ns's
    (map (fn [[k v]] [k (sort v)]) $)       ;; '([ns4 #{sorted-ns4-deps}], [ns1 #{sorted-ns1-deps}], ... )
    (sort-by first $)                       ;; '([ns1 #{sorted-ns1-deps}], [ns2 #{sorted-ns2-deps}], ... )
    {:dependencies $}))

(defn circular-reply
  "Finds all distinct circular dependencies on the classpath and
  returns them. This uses private, internal functions of
  `clojure.tools.namespace` and so it has a higher likihood of
  breaking than other functions."
  [& _]
  (as-> (#'dir/dirs-on-classpath) $
    (#'dir/find-files $)
    (#'file/files-and-deps $)
    (:depmap $)
    (cycle-paths $)
    (map (fn [v] [(set v) v]) $)   ;; uniqify
    (into {} $)  ;; uniqify
    (vals $)
    (map (fn [c] ["Circular Dependency Error" c]) $)
    {:dependencies $}))

(defn wrap-classpath
  "Middleware that provides the java classpath as well as dependency
  information."
  [handler]
  (with-safe-transport handler
    "classpath" classpath-reply
    "dependencies" dependencies-reply
    "circular" circular-reply))

(set-descriptor!
 #'wrap-classpath
 {:handles
  {"classpath"
   {:doc "Obtain a list of entries in the Java classpath."
    :returns {"classpath" "A list of the Java classpath entries."}}
   "dependencies"
   {:doc "Obtains a dependency tree for the namespaces on the classpath, or a single namespace provided in the `:ns` slot of `msg`."
    :optional {"ns" "The name of a single namespace to collect dependency information for."}
    :returns {"dependencies"
              "List of pairs, [head -> ns, tail -> ns's dependencies]."
              "status" "done"}}
   "circular"
   {:doc "Obtains a dependency tree for the namespaces with circular dependencies on the classpath."
    :optional {"ns" "The name of a single namespace to collect dependency information for."}
    :returns {"dependencies"
              "List of pairs, [head -> \"Circular Dependency\", tail -> ns path of circular dependency]."
              "status" "done"}}}})
