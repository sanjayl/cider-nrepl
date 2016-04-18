(ns cider.nrepl.middleware.dependencies
  (:require [cider.nrepl.middleware.util.error-handling :refer [with-safe-transport]]
            [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
            [clojure.tools.namespace.dir :as dir]
            [clojure.tools.namespace.file :as file]
            [clojure.tools.namespace.track :as track]))

(defn find-cycles
  "Returns the path of namespaces traversed to create a circular
  namespace level dependency."
  [cur {:keys [seen root stack graph] :as state}]
  (first (filter identity (for [c (remove seen cur)]
                            (if (= c root)
                              (conj stack c)
                              (find-cycles (get graph c) (-> state
                                                             (update-in [:stack] conj c)
                                                             (update-in [:seen] conj c))))))))

(defn uniquify-paths ;; TODO SANJAYL this doesn't work correctly for digraphs
  "Takes a collection of paths `paths` (which themselves are
  represented by ordered sequences of nodes to be traversed) and
  removes any paths that might be functionally equivalent to another
  path already in the collection. E.g, a list containing 2 equivalent
  circular paths such as `([a b c d e] [b c d e a])` will be reduced
  to a list with just one of these paths such as `([a b c d e])."
  [paths]
  (as-> paths $
    (zipmap (map set $) $)
    (vals $)))

(defn cycle-paths
  "Finds cycles in the directed dependency graph and then returns the
  path taken to create those cycles. The graph is represented by a
  map, where the `keys` are nodes and the `values` are sets of nodes
  depended-upon/pointed-to by the `key`. This method was itself
  pointed to by a Tweet by Chris Granger of Light Table fame:
  https://twitter.com/ibdknox/status/396409144090955776"
  [graph]
  (filterv identity
           (for [[root deps] graph
                 :let [stack (find-cycles deps {:seen #{} :stack [root] :graph graph :root root})]]
             stack)))

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
    (cycle-paths $) ;; [[cyc1], [equiv-cyc1], [cyc2], ...]
    (uniquify-paths $)
    (map (fn [c] ["Circular Dependency Error" c]) $)
    {:dependencies $}))

(defn dependencies-reply
  "If no `ns` is supplied in the `msg`, returns an alphabetically
  sorted list of pairs, with head being a namespace and tail being an
  alphabetically sorted list of head's dependencies. This will cover
  all the namespaces on the classpath.

  If a `ns` is given in the `msg`, then a similar data structure will
  be returned, but just for the ns provided. If `ns` cannot be found,
  then an empty list is returned.

  If an Exception is thrown due to a \"circular dependency error\",
  then the `circular-reply` function is automatically invoked and the
  circular path information is returned. If the `Exception` was thrown
  for other reasons, then it is re-thrown. Note that the `Exception`
  subtype is determined via analyzing the message, not the `type`, as
  such this functionality might be somewhat unstable if the underlying
  libraries change."
  [{:keys [ns] :as msg}]
  (try (as-> (dir/scan-all {}) $
         (get-in $ [::track/deps :dependencies]) ;; {ns4 #{ns4-deps}, ns1 #{ns1-deps} ... }
         (if ns
           (apply hash-map (find $ (symbol ns))) ;; just ns* -> filter: {ns* #{ns*-deps}}
           $)                             ;; otherwise keep all ns's
         (map (fn [[k v]] [k (sort v)]) $) ;; '([ns4 #{sorted-deps}], [ns1 #{sorted-deps}], ... )
         (sort-by first $) ;; '([ns1 #{sorted-deps}], [ns2 #{sorted-deps}], ... )
         {:dependencies $})
       (catch Exception e
         (if (.startsWith (.getMessage e) "Circular dependency between")
           (circular-reply)
           (throw e)))))

(defn wrap-dependencies
  "Middleware that provides namespace level dependency information."
  [handler]
  (with-safe-transport handler
    "dependencies" dependencies-reply
    "circular" circular-reply))

(set-descriptor!
 #'wrap-dependencies
 {:handles
  {"dependencies"
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
