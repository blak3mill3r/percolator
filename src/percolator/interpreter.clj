(in-ns 'percolator.core)

(def interpreters {})

(defn add-interpreters-to-scope [scope new-interpreters]
  (let [ existing (interpreters scope)
         merged   (merge existing new-interpreters) ]
    ( def interpreters (assoc interpreters scope merged))))

(defn reset-scope [scope]
  (merge interpreters
         {scope {}}))

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
      (root-interpreters interpreter-key) ))

(defn interpret-in-scope [scope form]
  (let [ root-interpreter       (interpreter-for-scope-and-form scope form)
        ; TODO scope inheritance, and multiple inheritance
         interpreter-arguments  (if (seq? form) (drop 1 form) [form]) ]
    (if root-interpreter
      ( let [ interpreter-result ( apply root-interpreter interpreter-arguments) ]
        ( if (interpreter-for-scope-and-form scope interpreter-result) (recur scope interpreter-result) interpreter-result ))
      ( let [ eval-result (eval form) ]
        ( if (interpreter-for-scope-and-form scope eval-result) (recur scope eval-result) eval-result )) ; recursively apply interpret-in-scope
        )))

;(interpret-in-scope :expression
;                    '('break)
;                    )


;(interpret-in-scope :expression
;                    5.0
;                    "FOobar"
;                    5
;                    )

