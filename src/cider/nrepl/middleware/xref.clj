(ns cider.nrepl.middleware.xref
  (:require [clojure.java.io :as io])
  (:import (clojure.lang RT)
           (java.io LineNumberReader InputStreamReader Reader PushbackReader)))

(defn- assoc-meta-with-fn
  "Takes a resolved function var FN, and returns an associative with
  some meta-data stuffed in."
  [fn]
  (let [m (meta fn)]
    (hash-map :fn fn
              :meta m
              :file (:file m)
              :line (:line m)
              :column  (:column m))))

(defn- read-source-from-a-file
  "Reads in a resource file specified by PATH, and parses Clojure
  objects specified by the list of function-info provided by
  RAW-FNS. TODO: Likely needs error handling."
  [[path raw-fns]]
  (let [fns        (remove #(nil? (:line %)) raw-fns)
        sorted-fns (sort-by (juxt :line :column) fns)]
    (when-let [uri (and path (io/resource path))]
      (with-open [r   (io/reader uri)
                  lr  (LineNumberReader. r)
                  pbr (PushbackReader. lr)]
        (loop [todo sorted-fns
               acc   '()]
          (if (empty? todo)
            acc
            (let [item    (first todo)
                  current (.getLineNumber lr)
                  target  (:line item)]
              (dotimes [_ (- target current 1)] (.readLine lr)) ;; burn lines
              (recur (rest todo)
                     (conj acc (assoc item :source (binding [*read-eval* false] ;; error handling here?
                                                     (read pbr))))))))))))

(defn- smash
  "Aggressively flattens collection X. In contrast to
  `clojure.core/flatten`, `smash` will flatten maps."
  [x]
  (remove coll? (rest (tree-seq coll? seq x))))

(defn- try-to-resolve
  "Tries to resolve item X in namespace NS. Will return item X
  unchanged if X is not a symbol."
  [ns x]
  (if (symbol? x) (ns-resolve ns x) x))

(defn- fn-uses-var?
  "Predicate that returns `true` if function FN-INFO's source-code
  utilizes the var V."
  [fn-info v]
  (let [ns              (->> fn-info :meta :ns)
        flat-source     (->> fn-info :source smash)
        resolved-source (map #(try-to-resolve ns %) flat-source)]
    (some #{v} resolved-source)))

(defn- search-ns-for-var
  "Finds all functions in the given namespace NS, reads in their
  source code, resolves the symbols to vars, and checks to see if the
  given var V matches any of the sourcecode."
  [ns v]
  (->> (->> ns ns-interns (map second) (filter ifn?)) ; all fns in ns
       (map assoc-meta-with-fn)          ; Keys- :fn, :meta, :file, :line, :column
       (group-by :file)                  ; One ns can be split over many files (clojure.core)
       (mapcat read-source-from-a-file)  ; Adds :source to the above keys
       (filter #(fn-uses-var? % v))))    ; Does var occur in the fn's source?

(defn- can-ns-resolve-var?
  "Predicate that returns `true` if namespace `given-ns` can utilize
  the fully namespace qualified var `var`; i.e., is `var` or `var's`
  ns required, used, or referred in `given-ns`."
  [given-ns var]
  (when-let [var-meta (meta var)]
    (let [var-sym (:name var-meta)
          var-ns  (:ns var-meta)]
      (or (= var (ns-resolve given-ns var-sym))           ;; require
          (->> given-ns ns-aliases vals (some #{var-ns})) ;; require as
          (->> given-ns ns-map vals (some #{var}))))))    ;; use/refer

(defn callers-of
  "Pass in a fully namespace QUALIFIED-NAME, and get a collection of
  maps, each map detailing a function that calls QUALIFIED-NAME. N.B.,
  currently, will not be able to find cross references for functions
  in `clojure.core`."
  [qualified-name]
  (let [v (resolve (symbol qualified-name))]
    (->> (all-ns)
         (filter #(can-ns-resolve-var? % (-> v meta :ns))) ; Narrow search space
         (mapcat #(search-ns-for-var % v))))) ; Searches the source code and returns the xrefs
