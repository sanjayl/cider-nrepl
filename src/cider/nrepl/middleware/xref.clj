(ns cider.nrepl.middleware.xref
  (:require [clojure.java.io :as io])
  (:import (clojure.lang RT)
           (java.io LineNumberReader InputStreamReader Reader PushbackReader)))

(defn- namespaces-that-call [sym]
  (->> (all-ns)
       (filter #(ns-resolve % sym))))

#_(defn- meta-with-source [sym]
    (assoc (meta sym) :source (source-code sym)))

(defn- fns-in-ns-that-call [ns target]
  (let [fns (->> ns ns-interns (filter ifn?))
        source (->> fns first :meta :source)]))

(defn- all-fns-in [ns]
  (->> ns ns-interns (map second) (filter ifn?)))

(defn- assoc-meta-with-fn [fn]
  (let [m (meta fn)]
    (hash-map :fn fn
              ;;:meta m  ;; TODO GET RID OF THIS??
              :file (:file m)
              :line (:line m)
              :column  (:column m))))

(defn- read-source-from-a-file [[path raw-fns]]
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
                     (conj acc (assoc item :source (binding [*read-eval* false]
                                                     (read pbr))))))))))))

(defn- does-fn-call? [fn-with-source target]
  (some #{target} (-> fn-with-source :source flatten)))

(defn- process-namespace [ns target]
  (as-> (all-fns-in ns) $              ; (fn1, fn2, fn3 ...)
    (map assoc-meta-with-fn $)         ; ({:fn fn1 :meta m1 :file f1 :line l1}, {:fn fn2 :meta m2 :file f2 :line l2} ...)
    (group-by :file $)                 ; ({file1 [{:fn 1..} {:fn 3..} {:fn 5..}]}, {file2 [{:fn 2..} {:fn 4..} {:fn 6..}]})
    (mapcat read-source-from-a-file $) ; [{:fn 1 :m m1 :source s1}, {:fn 2 :m m2 :source s2} ...]
    (filter #(does-fn-call? % target) $)))


(defn callers-of [t]
  (let [target (symbol t)]
    (->> (namespaces-that-call target)
         (mapcat #(process-namespace % target)))))



(comment (as-> (all-fns-in 'cider.nrepl.middleware.format) $ ; (fn1, fn2, fn3 ...)
           (map assoc-meta-with-fn $) ; ({:fn fn1 :meta m1 :file f1 :line l1}, {:fn fn2 :meta m2 :file f2 :line l2} ...)
           (group-by :file $) ; ({file1 [{:fn 1..} {:fn 3..} {:fn 5..}]}, {file2 [{:fn 2..} {:fn 4..} {:fn 6..}]})
           (mapcat read-source-from-a-file $) ; [{:fn 1 :m m1 :source s1}, {:fn 2 :m m2 :source s2} ...]
           (filter #(does-fn-call? % 'string/join) $)))



(comment 

  (defn callers-of [t]
    (let [target (symbol t)]
      (->> (namespaces-that-call target)
           (mapcat #(fns-in-ns-that-call % target))
           (map meta-with-source)
           (filter #(some #{target} (-> % :source flatten))))))



  (let [ns        (find-ns 'cider.nrepl.middleware.format)
        fns       (->> ns ns-interns vals (filter ifn?))
        ns-path   (->> fns first meta :file)
        fn-w-meta (map #(hash-map :fn % :meta (meta %) :line (:line (meta %))) fns) ;;can we remove the :meta?
        fn-lines  (->> fn-w-meta
                       (remove #(nil? (:line %)))
                       (sort-by :line))]
    (when-let [ns-uri (and ns-path (io/resource ns-path))]
      (with-open [r   (io/reader ns-uri)
                  lr  (LineNumberReader. r)
                  pbr (PushbackReader. lr)]
        (loop [rem fn-lines
               acc '()]
          (if (empty? rem)
            acc
            (let [item    (first rem)
                  current (.getLineNumber lr)
                  target  (:line item)]
              (dotimes [_ (- target current 1)] (.readLine lr))
              (recur (rest rem)
                     (conj acc (assoc item :source (binding [*read-eval* false]
                                                     (read pbr)))))))))))




  (let [ns        (find-ns 'clojure.core)
        fns       (->> ns ns-interns vals (filter ifn?))
        ns-path   (->> fns first meta :file)
        fn-w-meta (map #(hash-map :fn % :meta (meta %) :line (:line (meta %))) fns) ;;can we remove the :meta?
        fn-lines  (->> fn-w-meta
                       (remove #(nil? (:line %)))
                       (sort-by :line))]
    (->> (group-by #(get-in % [:meta :file]) fn-lines)
         first
         val)
    #_(when-let [ns-uri (and ns-path (io/resource ns-path))]
        (with-open [r   (io/reader ns-uri)
                    lr  (LineNumberReader. r)
                    pbr (PushbackReader. lr)]
          (loop [rem fn-lines
                 acc '()]
            (if (empty? rem)
              acc
              (let [item    (first rem)
                    current (.getLineNumber lr)
                    target  (:line item)]
                (dotimes [_ (- target current 1)] (.readLine lr))
                (recur (rest rem)
                       (conj acc (assoc item :source (binding [*read-eval* false]
                                                       (read pbr))))))))))))
