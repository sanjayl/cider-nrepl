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

(defn dependencies-reply [msg]
  (as-> (dir/scan-all {})
      $
    (get-in $ [::track/deps :dependencies])
    (into (sorted-map) $)))

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
