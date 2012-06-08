(in-ns 'percolator.core)
; give it a predicate and a coll of forms
; it returns the first form in the collection
; whose first element passes the predicate
(defn first-form-that-looks-like [predicate forms]
  (some
    #(when
       (when
         (seq? %1)
         (predicate (first %1)))
       %1)
    forms))

(defn partition-by-starts-with [predicate forms]
  (partition-by
    #(when
       (when
         (seq? %1)
         (predicate (first %1)))
       %1)
    forms))

(def modifiers-keywords
  { :public         ModifierSet/PUBLIC
    :private        ModifierSet/PRIVATE
    :protected      ModifierSet/PROTECTED
    :static         ModifierSet/STATIC
    :final          ModifierSet/FINAL
    :synchronized   ModifierSet/SYNCHRONIZED
    :volatile       ModifierSet/VOLATILE
    :transient      ModifierSet/TRANSIENT
    :native         ModifierSet/NATIVE
    :abstract       ModifierSet/ABSTRACT
    :strictfp       ModifierSet/STRICTFP
   })

(defn interpret-modifiers [form]
  (reduce bit-or 0 (map modifiers-keywords form)))

