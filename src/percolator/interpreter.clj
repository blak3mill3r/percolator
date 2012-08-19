(in-ns 'percolator.core)

(def interpreters {})

(defn add-interpreters-to-scope [scope interpreters]
  (let [ merge-into-scope (fn [x y] (do (println x) (println y))) ]
    ( def interpreters
      (merge-with merge-into-scope interpreters {scope interpreters}))))

;(add-interpreters-to-scope :statement {'break (fn [] `(new BreakStmt) ) })
;(reset-scope :statement)

(defn reset-scope [scope]
  (merge interpreters
         {scope {}}))

(defn interpreter-for-scope-and-form [scope form]
  (let [ root-interpreters (scope interpreters) ]
    (do
      ;(println (first form))
      (root-interpreters (first form)))))

; dispatch based on the class of the 2nd arg, the form
(defmulti interpret-in-scope #( class %2 ))

;(isa? clojure.lang.ASeq (class '(quote this)))

; multimethod so literals can potentially have special scope-local meaning
; this is just for forms
; but for it to work it'll have to handle some (clj) literals
(defmethod interpret-in-scope clojure.lang.ASeq [scope form]
  (let [ root-interpreter       (interpreter-for-scope-and-form scope form)
        ; TODO scope inheritance, and multiple inheritance
         interpreter-arguments  (drop 1 form) ]
    (if root-interpreter
      ( let [ interpreter-result ( apply root-interpreter interpreter-arguments) ]
        ( if (interpreter-for-scope-and-form scope interpreter-result) (recur scope interpreter-result) interpreter-result ))
      ( let [ eval-result (eval form) ]
        ( if (interpreter-for-scope-and-form scope eval-result) (recur scope eval-result) eval-result )) ; recursively apply interpret-in-scope
        )))


(defmethod interpret-in-scope java.lang.String    [scope string] `(new StringLiteralExpr  ~string))
(defmethod interpret-in-scope java.lang.Long      [scope long]   `(new LongLiteralExpr    ~(.toString long)))
(defmethod interpret-in-scope java.lang.Boolean   [scope bool]   `(new BooleanLiteralExpr ~bool))
(defmethod interpret-in-scope java.lang.Character [scope char]   `(new CharLiteralExpr    ~(.toString char)))
(defmethod interpret-in-scope java.lang.Double    [scope double] `(new DoubleLiteralExpr  ~(.toString double)))
(defmethod interpret-in-scope nil                 [scope & a]    `(new NullLiteralExpr ))

(reset-scope :expression)
(add-interpreters-to-scope
  :expression
  { '(quote break) (fn [] `(new BreakStmt) )
   })


(interpret-in-scope :expression
                    '('break)
                    )
