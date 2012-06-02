(ns nothing.foo)

# an example of macros

(defmacro def-logged-fn [fn-name args & body]
  `(defn ~fn-name ~args
     (println "Calling ...")
     ~@body))

(defmacro def-logged-fn [fn-name args & body]
  `(defn ~fn-name ~args
     ~@body))

(defn somethinglikeamacro [fn-name args & body]
  `(defn ~fn-name ~args
     ~@body))

(def-logged-fn say[name]
  (println
    (str "hello " name)))

(macroexpand '(def-logged-fn say[name] (println (str "hello " name))))

(say "foo")
