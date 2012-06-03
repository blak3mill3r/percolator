(ns percolator.statement
  (:use percolator.expression
        percolator.type
        percolator.util)
  (:import
    (japa.parser.ast.body AnnotationDeclaration 
                          AnnotationMemberDeclaration 
                          ClassOrInterfaceDeclaration 
                          ConstructorDeclaration 
                          EmptyMemberDeclaration 
                          EmptyTypeDeclaration 
                          EnumConstantDeclaration 
                          EnumDeclaration 
                          FieldDeclaration 
                          InitializerDeclaration 
                          MethodDeclaration
                          TypeDeclaration ; I think this is an abstract base
                          ModifierSet     ; that's like public and private and static and abstract and synchronized and final and all that shit
                          Parameter       ; as in a method declaration
                          VariableDeclarator
                          VariableDeclaratorId
                          )
(japa.parser ASTHelper)
(japa.parser.ast CompilationUnit)
(japa.parser.ast PackageDeclaration)
(japa.parser.ast.body AnnotationDeclaration 
                               AnnotationMemberDeclaration 
                               ClassOrInterfaceDeclaration 
                               ConstructorDeclaration 
                               EmptyMemberDeclaration 
                               EmptyTypeDeclaration 
                               EnumConstantDeclaration 
                               EnumDeclaration 
                               FieldDeclaration 
                               InitializerDeclaration 
                               MethodDeclaration
                               TypeDeclaration ; I think this is an abstract base
                               ModifierSet     ; that's like public and private and static and abstract and synchronized and final and all that shit
                               Parameter       ; as in a method declaration
                               VariableDeclarator
                               VariableDeclaratorId
                               )
(japa.parser.ast.stmt AssertStmt                            ; NOTYET
                               BlockStmt                             ; used, perhaps need a syntax for anonymous blocks
                               BreakStmt                             ; done, doesn't support identifying them uniquely which I think is only useful if you're using javaparser for modifying existing ASTs
                               ContinueStmt                          ; done, ditto unique identification
                               DoStmt                                ; done
                               EmptyStmt                             ; maybe not needed?
                               ExplicitConstructorInvocationStmt     ; TODO might kinda require class & method declarations 
                               ExpressionStmt                        ; done
                               ForeachStmt                           ; done
                               ForStmt                               ; done-ish doesn't support multiple expressions in initializer or updater
                               IfStmt                                ; done
                               LabeledStmt                           ; TODO think of a syntax for this
                               ReturnStmt                            ; done
                               SwitchEntryStmt                       ; done
                               SwitchStmt                            ; done
                               SynchronizedStmt                      ; NOTYET
                               ThrowStmt                             ; done
                               TryStmt                               ; NOTYET
                               TypeDeclarationStmt 
                               WhileStmt                             ; done
                               )
(japa.parser.ast.expr AnnotationExpr
                               ArrayAccessExpr 
                               ArrayCreationExpr 
                               ArrayInitializerExpr 
                               AssignExpr                       ; done
                               AssignExpr$Operator              ; done
                               BinaryExpr                       ; done
                               BinaryExpr$Operator              ; done
                               BooleanLiteralExpr               ; done
                               CastExpr 
                               CharLiteralExpr                  ; done
                               ClassExpr                        ; wtf
                               ConditionalExpr                  ; aka ternary TODO do if statement first
                               DoubleLiteralExpr                ; done
                               EnclosedExpr                     ; wtf
                               FieldAccessExpr                  ; done-ish with special /-in-a-symbol syntax (only possible if target is a NameExpr)
                               InstanceOfExpr 
                               IntegerLiteralExpr               ; not possible everything is a long, no biggie
                               IntegerLiteralMinValueExpr       ; wtf
                               LiteralExpr                      ; abstract
                               LongLiteralExpr                  ; done-ish ... better type inference from clojure primitives
                               LongLiteralMinValueExpr          ; wtf
                               MarkerAnnotationExpr 
                               MethodCallExpr                   ; done (anything missing?)
                               NameExpr                         ; done
                               NormalAnnotationExpr             ; wtf
                               NullLiteralExpr                  ; done
                               ObjectCreationExpr               ; done-ish, doesn't support outer/inner classes
                               QualifiedNameExpr                ; done-ish with the /-in-a-symbol syntax
                               SingleMemberAnnotationExpr 
                               StringLiteralExpr                ; done
                               SuperExpr                        ; done
                               ThisExpr                         ; done
                               UnaryExpr                        ; FIXME TODO
                               UnaryExpr$Operator               ; done
                               VariableDeclarationExpr          ; done-ish? can do a simple local variable ... lacking array types
                               )
( japa.parser.ast.type ClassOrInterfaceType 
                                PrimitiveType         ; a degrading term
                                PrimitiveType$Primitive
                                ReferenceType 
                                Type
                                VoidType              ; dude is going to build void rays
                                WildcardType )
    )
  )

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

;FIXME rename me
(defn interpret-switch-entry-statements [& entry-statements]
  (map interpret-switch-entry-statement entry-statements))

(defn interpret-statement-switch [expression & entries]
  `(new SwitchStmt
        ~(interpret-expression expression)
        [~@(map #(apply interpret-switch-entry-statement %1) entries)]))

(def statement-interpreters
  { '(quote return)   interpret-statement-return
    '(quote break)    interpret-statement-break
    '(quote continue) interpret-statement-continue
    '(quote throw)    interpret-statement-throw
    '(quote switch)   interpret-statement-switch
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

;(defmethod interpret-statement '(quote switch) [form]
;  (let [ expression (interpret-expression (nth form 1))
;         entries    (interpret-switch-entry-statements (nthrest form 2))
;        ]
;    `(new SwitchStmt ~expression ~entries )
;    ))
;
;
;    ;public SwitchEntryStmt(Expression label, List<Statement> stmts) {
;
;(defmethod interpret-statement '(quote for) [form]
;  (let [ init (interpret-expression (nth form 1))
;         condition  (interpret-expression (nth form 2))
;         update (interpret-expression (nth form 3))
;         body (interpret-block (nthrest form 4))
;         ]
;    `(new ForStmt
;          (doto (new java.util.ArrayList) (.add ~init))
;          ~condition
;          (doto (new java.util.ArrayList) (.add ~update))
;          ~body
;      )
;    ))
;
;(defmethod interpret-statement '(quote foreach) [form]
;  (let [ variable  (interpret-expression-variable-declaration (nth form 1))
;         interable (interpret-expression (nth form 2))
;         body      (interpret-block (nthrest form 3))
;         ]
;    `(new ForeachStmt ~variable ~interable ~body)))
;
;    ;;public ForeachStmt(VariableDeclarationExpr var, Expression iterable, Statement body) {
;
;(defmethod interpret-statement '(quote if) [form]
;  (let [ condition (interpret-expression (nth form 1))
;         if-block  (interpret-block (nth form 2))
;         else-block (interpret-block (first (nthrest form 3)))
;         ]
;    (if else-block
;      `(new IfStmt ~condition ~if-block ~else-block)
;      `(new IfStmt ~condition ~if-block nil)
;      )
;    ))
;
;(defmethod interpret-statement '(quote while) [form]
;  (let [ condition  (interpret-expression (nth form 1))
;         body       (interpret-block (nthrest form 2))
;         ]
;    `(new WhileStmt ~condition ~body)))
;
;(defmethod interpret-statement '(quote do-while) [form]
;  (let [ condition  (interpret-expression (nth form 1))
;         body       (interpret-block (nthrest form 2))
;         ]
;    `(new DoStmt ~body ~condition)))
;
;    ;public WhileStmt(Expression condition, Statement body) {
;
;(defmethod interpret-statement :default [form]
;  `(new ExpressionStmt ~(interpret-expression form)))
;

