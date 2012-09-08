(in-ns 'percolator.core)

(reset-scope :statement)

(defn interpret-statement [form] (interpret-in-scope :statement form))

(defn interpret-block [stmt-list]
  `(new BlockStmt [ ~@(map interpret-statement stmt-list) ] ))

(defn interpret-statement-return [& expression]
  (if (= nil (first expression))
    `(new japa.parser.ast.stmt.ReturnStmt)
    `(new japa.parser.ast.stmt.ReturnStmt
       ~(interpret-expression (first expression)))))

(defn interpret-statement-break    [] `(new BreakStmt))
(defn interpret-statement-continue [] `(new ContinueStmt))
(defn interpret-statement-throw [expression] `(new ThrowStmt ~(interpret-expression expression)))

;FIXME rename me
(defn interpret-switch-entry-statement [match-expression-or-default & statements]
  (let [ is-default       (= '(quote default) match-expression-or-default)
         match-expression (if is-default nil (interpret-expression match-expression-or-default)) ]
    `(new SwitchEntryStmt ~match-expression [ ~@(map interpret-statement statements) ] )))

(defn interpret-statement-switch [expression & entries]
  `(new SwitchStmt
        ~(interpret-expression expression)
        [~@(map #(apply interpret-switch-entry-statement %1) entries)]))

(defn interpret-statement-for [init condition update & body-statements]
  `(new ForStmt
        [~(apply interpret-expression-variable-declaration init) ]
        ~(interpret-expression condition)
        [ ~(interpret-expression update) ]
        ~(interpret-block body-statements)
        ))

(defn interpret-statement-foreach [variable iterable & body-statements]
  `(new ForeachStmt
        ~(apply interpret-expression-variable-declaration variable)
        ~(interpret-expression iterable)
        ~(interpret-block body-statements)))

(defn interpret-statement-if [condition if-block & else-block]
  (let [ else-block (first else-block) ] 
    (if else-block
      `(new IfStmt
            ~(interpret-expression condition)
            ~(interpret-block if-block)
            ~(interpret-block else-block))
      `(new IfStmt
            ~(interpret-expression condition)
            ~(interpret-block if-block)
            nil))))

(defn interpret-statement-while [condition & body]
  `(new WhileStmt
        ~(interpret-expression condition)
        ~(interpret-block body)))


(defn interpret-statement-do-while [condition & body]
  `(new DoStmt
        ~(interpret-block body)
        ~(interpret-expression condition)))

(defn interpret-statement-decl-class [& forms]
  `(new TypeDeclarationStmt ~( apply interpret-body-decl-class forms )))

(defn interpret-statement-block [& s] (interpret-block s))


(add-interpreters-to-scope
  :statement
  { 'return   interpret-statement-return
    'break    interpret-statement-break
    'continue interpret-statement-continue
    'throw    interpret-statement-throw
    'switch   interpret-statement-switch
    'for      interpret-statement-for
    'foreach  interpret-statement-foreach
    'if       interpret-statement-if
    'while    interpret-statement-while
    'do-while interpret-statement-do-while
    ; class type declaration statement
    'class    interpret-statement-decl-class
    ; just a code block ... (aka anonymous scope)
    'block    interpret-statement-block
    'empty    (interpreter [] `(new EmptyStmt))
   })

; statement inherits expression
; because any valid expression can become a statement
; by wrapping it in `(new ExpressionStmt ~expr)
; this is cool...
(inherit-scope :statement :expression (fn [expr] `(new ExpressionStmt ~expr)))
