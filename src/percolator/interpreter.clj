(in-ns 'percolator.core)

(def interpreters {})

(defn add-interpreters-to-scope [scope interpreters]
  (let [ merge-into-scope (fn [x y] (do (println x) (println y))) ]
    ( def interpreters
      (merge-with merge-into-scope interpreters {scope interpreters}))))

(defn reset-scope [scope]
  (merge interpreters
         {scope {}}))

(defn interpreter-for-scope-and-form [scope form]
  (let [ root-interpreters (scope interpreters)
         interpreter-key   (if (seq? form) (first form) (class form))
        ]
    (root-interpreters interpreter-key)))

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

(reset-scope :expression)
(add-interpreters-to-scope
  :expression
  { '(quote break)      (fn []              `(new BreakStmt)                               )
    java.lang.String    (fn [string]        `(new StringLiteralExpr ~string)               )
    java.lang.Long      (fn [long]          `(new LongLiteralExpr    ~(.toString long)    ))
    java.lang.Boolean   (fn [bool]          `(new BooleanLiteralExpr ~bool                ))
    java.lang.Character (fn [char]          `(new CharLiteralExpr    ~(.toString char)    ))
    java.lang.Double    (fn [double]        `(new DoubleLiteralExpr  ~(.toString double)  ))   ; to add floats as well will require some extra percolator syntax hint
    nil                 (fn [scope & a]     `(new NullLiteralExpr                          ))  ;; not 100% sure this is a good idea
   })

;(interpret-in-scope :expression
;                    '('break)
;                    )
;
;(interpret-in-scope :expression
;                    nil
;                    5.0
;                    "FOobar"
;                    )

