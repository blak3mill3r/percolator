(in-ns 'percolator.core)
(declare interpret-statement)

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

(def statement-interpreters
  { '(quote return)   interpret-statement-return
    '(quote break)    interpret-statement-break
    '(quote continue) interpret-statement-continue
    '(quote throw)    interpret-statement-throw
    '(quote switch)   interpret-statement-switch
    '(quote for)      interpret-statement-for
    '(quote foreach)  interpret-statement-foreach
    '(quote if)       interpret-statement-if
    '(quote while)    interpret-statement-while
    '(quote do-while) interpret-statement-do-while
    ; class type declaration statement
    '(quote class) interpret-statement-decl-class
    })

(defn interpret-statement [form]
  (let [ expression-interpreter (expression-interpreters (first form))
         statement-interpreter  (statement-interpreters  (first form))
         interpreter-arguments  (drop 1 form) ]
    (if expression-interpreter
      `(new ExpressionStmt ~( apply expression-interpreter interpreter-arguments ))
      (if statement-interpreter
        ( apply statement-interpreter interpreter-arguments )
        ; no matching expression or statement constructing syntax
        ; so eval it as a plain old clojure form
        ; and attempt to interpret the result as a statement form
        (interpret-statement (eval form))))))

; TODO try
;public TryStmt(BlockStmt tryBlock, List<CatchClause> catchs, BlockStmt finallyBlock) {
