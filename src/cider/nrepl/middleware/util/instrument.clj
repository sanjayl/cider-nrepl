(ns cider.nrepl.middleware.util.instrument
  "Generic instrumentation for clojure code"
  {:author "Artur Malabarba"}
  (:require [cider.nrepl.middleware.util.meta :as m]
            [clojure.walk :as walk]))

;;;; # Instrumentation
;;; The following code is responsible for automatic instrumentation.
;;; This involves:
;;;    - knowing what's interesting and what's not,
;;;    - walking though the code,
;;;    - distinguishing function calls from special-forms,
;;;    - distinguishing between the different collections.

;;;; ## Auxiliary defs
(def irrelevant-return-value-forms
  "Set of macros whose return value we don't care about.
  When instrumenting, these will not be wrapped in a breakpoint."
  '#{def fn* deftype* reify* quote
     catch finally
     monitor-enter monitor-exit})

;;; Surprisingly, (list? `(inc 1)) is false.
(defn- listy?
  "Check if `x` is a list, LazySeq or Cons."
  [x]
  (or (list? x)
      (instance? clojure.lang.LazySeq x)
      (instance? clojure.lang.Cons x)))

;;;; ## Instrumentation
;;; The top-level instrumenting function is `read-and-instrument`. See
;;; its doc for more information.
;;;
;;; Each of the other `instrument-*` functions is responsible for
;;; calling subordinates and incrementing the coordinates vector if
;;; necessary.

;;;; ### Instrumenting Special forms
;;; Here, we implement instrumentation of special-forms on a
;;; case-by-case basis. Unlike function calls, we can't just look at
;;; each argument separately.
(declare instrument)

(defn- instrument-coll
  "Instrument a collection."
  [coll]
  (m/merge-meta (walk/walk instrument identity coll)
    (meta coll)))

(defn instrument-case-map
  "Instrument the map that is 5th arg in a `case*`."
  [args]
  (into {} (map (fn [[k [v1 v2]]] [k [v1 (instrument v2)]])
                args)))

(defn- instrument-special-form
  "Instrument form representing a macro call or special-form."
  [[name & args :as form]]
  (cons name
        ;; We're dealing with some low level stuff here, and some of
        ;; these internal forms are completely undocumented, so let's
        ;; play it safe and use a `try`.
        (try
          (condp #(%1 %2) name
            '#{if do recur throw finally try new monitor-exit monitor-enter} (instrument-coll args)
            '#{quote & var clojure.core/import*} args
            '#{.} (list* (instrument (first args))
                         (second args)
                         (instrument-coll (rest (rest args))))
            '#{def} (let [sym (first args)]
                      (list* (m/merge-meta sym
                               (instrument (or (meta sym) {}))
                               {::instrumented true})
                             (map instrument (rest args))))
            '#{set!} (list (m/merge-meta (first args)
                             {::instrumented true})
                           (instrument (second args)))
            '#{loop* let* letfn*} (cons (-> (fn [i x] (if (odd? i) (instrument x) x))
                                            (map-indexed (first args))
                                            vec)
                                        (instrument-coll (rest args)))
            '#{reify* deftype*} (map #(if (listy? %)
                                        (let [[a1 a2 & ar] %]
                                          (m/merge-meta (list* a1 a2 (instrument-coll ar))
                                            (meta %)))
                                        %)
                                     args)
            ;; `fn*` has several possible syntaxes.
            '#{fn*} (let [[a1 & [a2 & ar :as a1r]] args]
                      (cond
                        (vector? a1)       (cons a1 (instrument-coll a1r))
                        (and (symbol? a1)
                             (vector? a2)) (list* a1 a2 (instrument-coll ar))
                        :else              (map #(if (listy? %)
                                                   (-> (first %)
                                                       (cons (instrument-coll (rest %)))
                                                       (m/merge-meta (meta %)))
                                                   %)
                                                args)))
            '#{catch} `(~@(take 2 args)
                        ~@(instrument-coll (drop 2 args)))
            ;; Anyone know what a2 and a3 represent? They were always 0 on my tests.
            '#{case*} (let [[a1 a2 a3 a4 a5 & ar] args]
                        `(~a1 ~a2 ~a3 ~(instrument a4) ~(instrument-case-map a5) ~@ar)))
          (catch Exception e
            (binding [*print-length* 4
                      *print-level*  2]
              (println "Failed to instrument" name args
                       ", please file a bug report: " e))
            args))))

;;;; ### Instrumenting Functions and Collections
;;; This part is quite simple, most of the code is devoted to checking
;;; form-types and special cases. The idea here is that we walk
;;; through collections and function arguments looking for interesting
;;; things around which we'll wrap a breakpoint. Interesting things
;;; are most function-forms and vars.
(defn- instrument-function-call
  "Instrument a regular function call sexp.
  This must be a sexp that starts with a symbol which is not a macro
  nor a special form.
  This includes regular function forms, like `(range 10)`, and also
  includes calls to Java methods, like `(System/currentTimeMillis)`."
  [[name & args]]
  (cons name (instrument-coll args)))

(def verbose-debug false)

(defn with-break
  "Return form wrapped in a breakpoint.
  If function is given, use it to instrument form before wrapping. The
  breakpoint is given by the form's ::breakfunction metadata."
  ([function form]
   (with-break (m/merge-meta (function form) (meta form))))
  ([form]
   (let [{coor ::coor,
          orig ::original-form,
          bf   ::breakfunction} (meta form)]
     (when verbose-debug
       (println "[DBG]" (not (not bf)) coor (or orig form)))
     (cond
       (and bf (seq coor))
       (list bf form coor orig)
       ;; If the form is a list and has no metadata, maybe it was
       ;; destroyed by a macro. Try guessing the coor by looking at
       ;; the first element. This fixes `->`, for instance.
       (listy? form)
       (let [{coor ::coor,
              orig ::original-form,
              bf   ::breakfunction} (meta (first form))
             coor (if (= (last coor) 0)
                    (pop coor)
                    coor)]
         (if (and bf (seq coor))
           (list bf form coor orig)
           form))
       :else form))))

(defn- contains-recur?
  "Return true if form is not a `loop` and a `recur` is found in it."
  [form]
  (if (listy? form)
    (case (first form)
      recur true
      loop  false
      (some contains-recur? (rest form)))))

(defn- dont-break?
  "Return true if it's NOT ok to wrap form in a breakpoint.
  Expressions we don't want to wrap are those listed in
  `irrelevant-return-value-forms` and anything containing a `recur`
  form (unless it's inside a `loop`)."
  [[name :as form]]
  (or (irrelevant-return-value-forms name)
      (contains-recur? form)))

(defn- instrument-function-like-form
  "Instrument form representing a function call or special-form."
  [[name :as form]]
  (if-not (symbol? name)
    ;; If the car is not a symbol, nothing fancy is going on and we
    ;; can instrument everything.
    (with-break instrument-coll form)
    (if (special-symbol? name)
      ;; If special form, thread with care.
      (if (dont-break? form)
        (instrument-special-form form)
        (with-break instrument-special-form form))
      ;; Otherwise, probably just a function. Just leave the
      ;; function name and instrument the args.
      (with-break instrument-function-call form))))

(defn instrument
  "Walk through form and return it instrumented with breakpoints.
  Only forms with a ::breakfunction metadata will be
  instrumented, and even then only if it makes sense (e.g., the
  binding names in a let form are never instrumented).
  See `read-and-instrument` for more information."
  [form]
  (condp #(%1 %2) form
    ;; Function call, macro call, or special form.
    listy? (doall (instrument-function-like-form form))
    symbol? (with-break form)
    ;; Other coll types are safe, so we go inside them and only
    ;; instrument what's interesting.
    ;; Do we also need to check for seq?
    coll? (doall (instrument-coll form))
    ;; Other things are uninteresting, literals or unreadable objects.
    form))

;;;; ## Pre-instrumentation
;;; The following functions are used to populate with metadata and
;;; macroexpand code before instrumenting it. This is where we
;;; calculate all the ::coor vectors. See `read-and-instrument`.
(defn walk-indexed
  "Walk through form calling (f coor element).
  The value of coor is a vector of indices representing element's
  address in the form. Unlike `clojure.walk/walk`, all metadata of
  objects in the form is preserved."
  ([f form] (walk-indexed [] f form))
  ([coor f form]
   (let [map-inner (fn [forms]
                     (map-indexed #(walk-indexed (conj coor %1) f %2)
                                  forms))
         ;; Maps are unordered, but we can try to use the keys (and
         ;; they're important enough that we're willing to risk
         ;; getting the position wrong).
         result (cond (map? form)  (into {} (map (fn [[k v]]
                                                   [k (walk-indexed (conj coor (pr-str k)) f v)])
                                                 form))
                      ;; Order of sets is unpredictable, unfortunately.
                      (set? form)  form
                      ;; Borrowed from clojure.walk/walk
                      (list? form) (apply list (map-inner form))
                      (instance? clojure.lang.IMapEntry form) (vec (map-inner form))
                      (seq? form)  (doall (map-inner form))
                      (coll? form) (into (empty form) (map-inner form))
                      :else form)]
     (f coor (m/merge-meta result (meta form))))))

(defn tag-form
  "Tag form to be instrumented with breakfunction.
  This sets the ::breakfunction metadata of form, which can then be
  used by `instrument-tagged-code`. See this function for the meaning
  of breakfunction."
  [form breakfunction]
  (m/merge-meta form {::breakfunction breakfunction}))

(defn instrument-tagged-code
  "Return `form` instrumented with breakpoints.
  It is expected that something in `form` will contain a
  ::breakfunction metadata, whose value should be a var holding a
  macro. This macro should take three arguments, the form being
  evaluated, a coordinates vector (see below), and the original
  form (before macroexpansion).

  This function walks through the code attaching to objects
  the ::coor metadata, which is a vector specifing its position
  in the top-level form. As an example, a coordinate vector of [3 2 0]
  means:
    - enter this sexp and move forward three times,
    - enter this sexp and move forward twice,
    - enter this sexp.

  After that, it fully macroexpands the code, walks through it again,
  and wraps in a breakpoint any form that contains the previously
  attached metadata."
  [form]
  (->> form
       ;; Fill the form with metadata. This will later tell us which
       ;; of the final (post-expansion) forms correspond to user
       ;; code (and where it came from).
       (walk-indexed (fn [i f]
                       (m/merge-meta f
                         {::original-form (m/strip-meta f)
                          ::coor i})))
       ;; Expand so we don't have to deal with macros.
       (m/macroexpand-all)
       ;; Go through everything again, and instrument any form with
       ;; debug metadata.
       (instrument)
       (#(do (when verbose-debug (println "[DBG]" %))
             %))))

(defn list-instrumented-defs [ns]
  (let [ns (if (instance? clojure.lang.Namespace ns) ns
               (find-ns (symbol ns)))]
    (->> (ns-interns ns)
         (filter (comp ::instrumented meta second))
         (map first))))
