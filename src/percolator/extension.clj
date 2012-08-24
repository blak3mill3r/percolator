(in-ns 'percolator.core)

; this file defines two macros 'interpreter' and 'definterpreter'
; they are structurally a lot like clojure.core/fn and clojure.core/defn
; these two macros are used to extend percolator
; building forms which work sort of like clojure macros
; by using backquotes and splicing in the body of an 'interpreter'
; you can get macro-power for extending java-building code with percolator
; that's the idea... I'm not 100% satisfied with the implementation below

; meaning, not having to type ~' before each unqualified symbol in a backquote form in an 'interpreter' body
; I don't know if there's a more sensible way to do this, but this does work and the dirtiness is at least contained :)
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


; this is similar to the clojure.core/fn macro
; except that unqualified symbols in backquote forms are treated differently
; as they represent splicing of percolator forms
; the difference between this and fn is that the unqualified symbols in
; backquote forms need to STAY unqualified, they are
; NOT resolved in the current namespace as they would be with fn
; because they will later be interpreted by percolator which treats
; unqualified symbols specially in order to keep the percolator syntax clean
;
; I think this makes some sense
; feedback welcome
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
      (with-meta
      (if name
        (list* 'fn* name newsigs)
        (cons 'fn* newsigs))
      (meta &form)) ))

; this is to interpreter what defn is to fn
; it's duplicating most of defn
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

