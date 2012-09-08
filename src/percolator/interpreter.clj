(in-ns 'percolator.core)

(def interpreters {})
(def scope-inheritance {})

(defn add-interpreters-to-scope [scope new-interpreters]
  (let [ existing (interpreters scope)
         merged   (merge existing new-interpreters) ]
    ( def interpreters (assoc interpreters scope merged))))

(defn inherit-scope [scope parent wrapper]
  (let [ existing  (scope-inheritance scope)
         modified  (conj existing [parent wrapper])
        ]
    (def scope-inheritance
      (assoc scope-inheritance scope modified))))

(defn reset-scope [scope]
  (def interpreters
    (assoc interpreters scope {}))
  (def scope-inheritance
    (assoc scope-inheritance scope [])))

; NOTE the ordering matters if interpreter keys shadow each other
; the LAST one you add with inherit-scope will take precedence in the event of a collision
; undecided if that's Good Thing or not, maybe it should be other way around
; in which case this is the place to start
(defn inherited-interpreters [scope]
  (let [ inherited-scopes (scope-inheritance scope)
         interpreters-for-scopes 
         (map #( interpreters ( first %1 ) ) inherited-scopes)
        ]
    (reduce merge {} interpreters-for-scopes) ))

(defn inherited-interpreter-wrapper [scope parent-scope]
  (or
    ((reduce merge {} (scope-inheritance scope))
     parent-scope)
    identity ))

; a map of scope names to
; maps of syntax keys to the (originating) scope name
(defn scope-inheritance-name-map []
  (reduce merge {} 
    (reverse
      (map (fn [a] (reduce
                     (fn [map-to-scope-name [k v]] (assoc map-to-scope-name k (first a)))
                     {}
                     (last a)))
      interpreters))))

(defn extract-interpreter-key-from-form [form]
  (if
    (seq? form)
    (if
      ( = 'quote (first form) ) 
        (last form))))

(defn interpreter-for-scope-and-form [scope form]
  (let [ root-interpreters (or (scope interpreters) {})
         interpreter-key   (if (seq? form) (extract-interpreter-key-from-form (first form)) (class form))
        ]
    (or
      (root-interpreters interpreter-key)
      (let [ inherited
               (( inherited-interpreters scope) interpreter-key)
             scope-inherited-from
               ( (scope-inheritance-name-map) interpreter-key )
             wrapper
               ( inherited-interpreter-wrapper scope scope-inherited-from )
            ]
        (if inherited
          (fn [& args] (wrapper (apply inherited args))))))))

(def ^:dynamic *perc-scope-args* {})

(defn interpret-in-scope [scope form]
  (let [ interpreter            (interpreter-for-scope-and-form scope form)
         interpreter-arguments  (if (seq? form) (drop 1 form) [form])
         looks-percolatorish    (and (seq? form) (seq? (first form) ) (= 'quote (first (first form) )) ) ]  ; if no interpreter is found this is used to know whether to error out or eval the form
    (if interpreter
      ( let [ interpreter-result ( apply interpreter interpreter-arguments) ]
        ( if (interpreter-for-scope-and-form scope interpreter-result)
          (recur scope interpreter-result) interpreter-result ))
      (if looks-percolatorish (println "PROBLEM interpreting, in scope (" scope ") this form:\n" form)
        ( let [ eval-result (eval form) ]
        ( if (interpreter-for-scope-and-form scope eval-result)
          (recur scope eval-result) eval-result ))))))
