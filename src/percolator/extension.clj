(in-ns 'percolator.core)

;trickery (dirty hack) to allow more comfortable interpreter definition syntax

(defn turn-unqualified-symbols-back-into-unqualified-symbols [form ns-name]
  (let [ qualifier-stripper
        (fn [symbol]
          (last
            (re-find
              (re-pattern
                (apply
                  str
                  (concat
                    (.toString ns-name)
                    (seq "/(.*)" ))))
              (.toString symbol))))
          recurse-preserving-ns
          #( turn-unqualified-symbols-back-into-unqualified-symbols %1 ns-name )
        ]
    (if (seq? form)
      (if (= (first form) 'quote)
        (let [ maybe-unqualified-symbol-as-string (qualifier-stripper (nth form 1)) ]
          (if maybe-unqualified-symbol-as-string
            `(quote ~(symbol (qualifier-stripper (nth form 1)) ))
            form ; was a symbol but not one we're looking for
            ))
        (map recurse-preserving-ns form)) ; was a sequence but not one like (quote asymbol)
      form))) ; was not a sequence


; the definition of the fn macro
; is something like what I want 
; at least in terms of args accepted
(defmacro interpreter
  "params => positional-params* , or positional-params* & next-param
  positional-param => binding-form
  next-param => binding-form
  name => symbol

  Defines a Percolator interpreter extension"
  { :forms '[(interpreter name? [params* ] exprs*)] }
  [& sigs]
    (let [name (if (symbol? (first sigs)) (first sigs) nil)
          sigs (if name (next sigs) sigs)
          newsigs (map #(turn-unqualified-symbols-back-into-unqualified-symbols %1 *ns*) sigs)
          ]
      (do
        (doall (map println [ name sigs newsigs ]))
        (with-meta
        (if name
          (list* 'fn* name newsigs)
          (cons 'fn* newsigs))
        (meta &form)) )))


(def 

 ^{:macro true
   :doc "Same as (def name (interpreter [params* ] exprs*)) or (def
         name (interpreter ([params* ] exprs*)+))
         Note, this is missing the argslist-into-metadata bit of clojure's defn macro because I didn't feel like it"
   :arglists '([name doc-string? attr-map? [params*] body]
                [name doc-string? attr-map? ([params*] body)+ attr-map?])
   :added "1.0"}
 definterpreter (fn definterpreter [&form &env name & fdecl]
        (let [m (if (string? (first fdecl))
                  {:doc (first fdecl)}
                  {})
              fdecl (if (string? (first fdecl))
                      (next fdecl)
                      fdecl)
              m (if (map? (first fdecl))
                  (conj m (first fdecl))
                  m)
              fdecl (if (map? (first fdecl))
                      (next fdecl)
                      fdecl)
              fdecl (if (vector? (first fdecl))
                      (list fdecl)
                      fdecl)
              m (if (map? (last fdecl))
                  (conj m (last fdecl))
                  m)
              fdecl (if (map? (last fdecl))
                      (butlast fdecl)
                      fdecl)
              m (let [inline (:inline m)
                      ifn (first inline)
                      iname (second inline)]
                  ;; same as: (if (and (= 'fn ifn) (not (symbol? iname))) ...)
                  (if (if (clojure.lang.Util/equiv 'interpreter ifn)
                        (if (instance? clojure.lang.Symbol iname) false true))
                    ;; inserts the same fn name to the inline fn if it does not have one
                    (assoc m :inline (cons ifn (cons (clojure.lang.Symbol/intern (.concat (.getName ^clojure.lang.Symbol name) "__inliner"))
                                                     (next inline))))
                    m))
              m (conj (if (meta name) (meta name) {}) m)]
          (list 'def (with-meta name m)
                ;;todo - restore propagation of fn name
                ;;must figure out how to convey primitive hints to self calls first
                (cons `interpreter fdecl) ))))

