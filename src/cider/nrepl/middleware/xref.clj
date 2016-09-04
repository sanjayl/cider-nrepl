(ns cider.nrepl.middleware.xref
  (:import (clojure.lang RT)
           (java.io LineNumberReader InputStreamReader Reader PushbackReader)))

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
