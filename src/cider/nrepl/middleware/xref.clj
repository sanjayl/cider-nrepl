(ns cider.nrepl.middleware.xref
  (:require [clojure.walk :as walk])
  (:import (clojure.lang RT)
           (java.io LineNumberReader InputStreamReader Reader PushbackReader)))

(defn- get-source-from-var
  "Returns a string of the source code for the given symbol, if it can
  find it. This requires that the symbol resolve to a Var defined in
  a namespace for which the .clj is in the classpath. Returns nil if
  it can't find the source.
  Example: (get-source-from-var 'filter)"

  [v] (when-let [filepath (:file (meta v))]
        (when-let [strm (.getResourceAsStream (RT/baseLoader) filepath)]
          (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
            (dotimes [_ (dec (:line (meta v)))] (.readLine rdr))
            (let [text (StringBuilder.)
                  pbr (proxy [PushbackReader] [rdr]
                        (read [] (let [#^Reader this this
                                       i (proxy-super read)]
                                   (.append text (char i))
                                   i)))]
              (read (PushbackReader. pbr))
              (str text))))))



(defn- recursive-contains? [coll obj]
  "True if coll contains obj. Obj can't be a seq"
  (not (empty? (filter #(= obj %) (flatten coll)))))


(defn- does-var-call-fn [var fn]
  "Checks if a var calls a function named 'fn"
  (if-let [source (get-source-from-var var)]
    (let [node (read-string source)]
      (if (recursive-contains? node fn)
        var
        false))))

(defn- does-ns-refer-to-var? [ns var]
  (ns-resolve ns var))

(defn all-vars-who-call-2 [sym]
  (filter
   ifn?
   (filter
    #(identity %)
    (map #(does-var-call-fn % sym)
         (flatten
          (map vals
               (map ns-interns
                    (filter #(does-ns-refer-to-var? % sym)
                            (all-ns)))))))))


(defn all-vars-who-call [sym]
  (as-> (all-ns)
      $
    (filter #(does-ns-refer-to-var? % sym) $)
    (map ns-interns $)
    (map vals $)
    (flatten $)
    (map #(does-var-call-fn % sym) $)
    (filter #(identity %) $)
    (filter ifn? $)))

(as-> (all-ns)
    $
  (filter #(ns-resolve % sym) $))
