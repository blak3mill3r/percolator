(in-ns 'percolator.core)

(defn first-matches [predicate]
  (fn [coll] 
    (when (when (seq? coll) (predicate (first coll))) coll)))

; give it a predicate and a coll of forms
; it returns the first form in the collection
; whose first element passes the predicate
(defn first-form-that-looks-like [predicate forms]
  (some (first-matches predicate) forms))

(defn partition-by-starts-with [predicate forms]
  (partition-by (first-matches predicate) forms))

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

; modifiers and annotations conveniently share the same home in percolator code
; FIXME rename this extract-modifiers-annotations-and-throws
; or something
(defn extract-modifiers-and-annotations [form]
  (let [ throws-lookin (filter #(symbol? %) form)
         modifier-lookin ( filter #(keyword? %) form )
         annotation-lookin (first (filter #(vector? %) form )) ]
    {:modifiers (reduce bit-or 0 (map modifiers-keywords modifier-lookin)) 
     :annotations annotation-lookin
     :throws (when-not (empty? throws-lookin) `[ ~@( map (fn [s] `(new NameExpr ~(.toString s))) throws-lookin)] )
     } ))
