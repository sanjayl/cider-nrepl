(ns cider.nrepl.middleware.xref
  (:require [clojure.java.io :as io])
  (:import (clojure.lang RT)
           (java.io LineNumberReader InputStreamReader Reader PushbackReader)))

(defn- source-code-of [var]
  (when-let [path (:file (meta var))]
    (with-open [r   (io/reader path) ;; TODO: FIGURE OUT RESOURCE OR PATH HERE
                pbr (java.io.PushbackReader. r)]
      (dotimes [_ (-> var meta :line dec)] (.readLine r))
      (binding [*read-eval* false]
        (read pbr)))))

(defn- candidate-xrefs [sym]
  (->> (all-ns)
       (filter #(ns-resolve % (symbol sym)))
       (map (comp vals ns-interns))
       flatten))

(defn callers-of [fn-name]
  (let [f (symbol fn-name)]
    (->> (candidate-xrefs f)
         (filter ifn?)
         (filter (fn [xref] (some #{f} (-> xref source-code-of flatten)))))))


;;;;;;;;;;;;;;;;;;;;;;   OLD ;;;;;;;;;;;;;;;;;;;;;;;;


(defn- get-source-from-var
  "Returns a string of the source code for the given symbol, if it can
  find it. This requires that the symbol resolve to a Var defined in
  a namespace for which the .clj is in the classpath. Returns nil if
  it can't find the source.
  Example: (get-source-from-var 'filter)"
  [v]
  (when-let [filepath (:file (meta v))]
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
  (when-let [source (get-source-from-var var)]
    (when (recursive-contains? (read-string source) fn)
      var)))

(defn all-vars-who-call [sym]
  (as-> (all-ns)
      $
    (filter #(ns-resolve % sym) $)
    (map (comp vals ns-interns) $)
    (flatten $)
    (map #(does-var-call-fn % sym) $)
    (filter ifn? $)))
