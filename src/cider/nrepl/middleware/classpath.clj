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
  "Returns a sorted map, with keys being the namespaces on the
  classpath, and values being a sorted set of that namespace's
  dependencies. (Note: requires a single arity function since the
  `with-safe-transport` macro expects to pass the `msg` to all
  `<op>-reply` functions.)"
  [_]
  (as-> (dir/scan-all {}) $
    (get-in $ [::track/deps :dependencies])
    (map (fn [[k v]] [k (into (sorted-set) v)]) $)
    (into (sorted-map) $)
    {:dependencies $}))

(defn wrap-classpath
  "Middleware that provides the java classpath."
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
   {:doc "Obtains a dependency tree for the namespaces on the classpath"
    :returns {"dependencies"
              "Map with keys -> namespaces on classpath, values -> set of that ns's dependencies.",
              "status" "done"}}}})
