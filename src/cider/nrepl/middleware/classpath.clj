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
  "Returns an alphabetically sorted list of pairs, with head being a
  namespace on the classpath, and tail being an alphabetically sorted
  list of head's dependencies. (Note: the `with-safe-transport` macro
  expects this to be a single arity function, passing the `msg` in. We
  don't need it, hence the atypical signature."
  [& _]
  (as-> (dir/scan-all {}) $
    (get-in $ [::track/deps :dependencies])
    (map (fn [[k v]] [k (sort v)]) $)
    (sort-by first $)
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
   {:doc "Obtains a dependency tree for the namespaces on the classpath"
    :returns {"dependencies"
              "List of pairs, [head -> ns, tail -> ns's dependencies]."
              "status" "done"}}}})
