(ns cider.nrepl.middleware.xref
  (:require [clojure.java.io :as io])
  (:import (clojure.lang RT)
           (java.io LineNumberReader InputStreamReader Reader PushbackReader)))

(defn- source-code [var]
  (when-let [path (:file (meta var))]
    (when-let [uri (io/resource path)]
      (with-open [r   (io/reader uri)
                  pbr (java.io.PushbackReader. r)]
        (dotimes [_ (-> var meta :line dec)] (.readLine r))
        (binding [*read-eval* false]
          (try (read pbr)
               (catch Exception E)))))))

(defn- possible-callers-of [sym]
  (->> (all-ns)
       (filter #(ns-resolve % sym))
       (map (comp vals ns-interns))
       flatten))

(defn- meta-with-source [sym]
  (assoc (meta sym) :source (source-code sym)))

(defn callers-of [t]
  (let [target (symbol t)]
    (->> (possible-callers-of target)
         (filter ifn?)
         (map meta-with-source)
         (filter #(some #{target} (-> % :source flatten))))))
