(ns percolator.declaration
  (:use percolator.expression
        percolator.type
        percolator.util
        percolator.statement)
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
(defn interpret-parameter [form]
  (let [ param-type             (reference-type (interpret-type (nth form 0)))
         param-name             (.toString (nth form 1))
         array-count-or-varargs (first (nthrest form 2))
         param-construction     `( ASTHelper/createParameter ~param-type ~param-name )
       ]
    (case array-count-or-varargs
      ...  `(doto ~param-construction (.setVarArgs true))
      nil  param-construction)))

(interpret-parameter 
  '(
    String args ...
    ))


(defn interpret-method-decl [form]
  (let [ modifiers   (interpret-modifiers    (nth form 1))
         return-type (interpret-type         (nth form 2))
         method-name                         (nth form 3)
         param-list (map interpret-parameter (nth form 4))
         body          (interpret-block  (nthrest form 5))
       ]
    `(doto
       (new MethodDeclaration ~modifiers ~return-type ~method-name [~@param-list])
       (.setBody ~body))))

(defmulti interpret-body-decl first)
(defmethod interpret-body-decl '(quote decl-method) [form] (interpret-method-decl form))

(defn interpret-class-decl [form]
  (let [ modifiers (interpret-modifiers (nth form 1))
         class-name (nth form 2)
         body-decls (map interpret-body-decl (nthrest form 3))
         ]
    `( new ClassOrInterfaceDeclaration
          nil ; javadoc
          ~modifiers
          nil ; annotations
          false ; isInterface
          ~class-name
          nil ; list of TypeParameter
          nil ; extends list
          nil ; implements list
          [~@body-decls] )))

(comment 
(interpret-class-decl '(
                        'decl-class
                        #{:public :final}
                        "MySickClass"
                        ( 'decl-method #{:private :synchronized} java.lang.String<x> "headbang" [(int x) (int y) (String args ...)] ('return x) )
                        ))
  )


