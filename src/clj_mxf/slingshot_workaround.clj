(ns clj-mxf.slingshot-workaround
  (:use clojure.core)           ; need 1.4.0 for ex-info and ex-data
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:gen-class))

(defmacro try-assoc+
  "Workaround for bug in slingshot where you can't throw+ inside the catch block
   of a try+.  If not thrown by throw+, the exception is still caught, but the
   original exception is wrapped and is accessed by the key :inner-exception.

   Syntax: (try-assoc+ <body> (rethrow-assoc :key1 val1, :key2 val2))"
  [& body]
  (let  [real-body (drop-last body),
         last-form (last body),
         keyvals (when (and (seq? last-form)
                            (= (first last-form) 'rethrow-assoc))
                   (rest last-form))]
   `(try
      ~@(if keyvals
          (seq
            (conj (vec real-body)
                  `(catch Throwable e#
                     (let [m# (assoc {} ~@keyvals),
                           ex-data# (if (instance? RuntimeException e#)
                                          (update-in (ex-data e#) [:object] merge m#)
                                          (assoc {} :object (assoc m# :inner-exception e#)
                                                    :environment {}))
                           obj# (:object ex-data#)]
                       (throw (ex-info (format "throw+: %s" (str obj#)) 
                                       ex-data#))))))
          body))))
