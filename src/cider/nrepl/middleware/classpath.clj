(ns cider.nrepl.middleware.classpath
  (:require [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [clojure.java.classpath :as cp]
            [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
            [clojure.tools.namespace.dir :as dir]
            [clojure.tools.namespace.track :as track]))

(defn classpath []
  (map str (cp/classpath)))

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

(defn wrap-classpath
  "Middleware that provides the java classpath as well as dependency
  information."
  [handler]
  (with-safe-transport handler
    "classpath" classpath-reply
    "dependencies" dependencies-reply))

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
              "status" "done"}}}})
