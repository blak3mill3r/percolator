(ns percolator.core
  (:use percolator.expression
        percolator.type
        percolator.util
        percolator.statement
        percolator.declaration)
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
(japa.parser.ast CompilationUnit
                 PackageDeclaration)
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

(defn wrap-a-class-kluge [class-decl]
  (let [ cu (new CompilationUnit) ]
    (.setPackage cu (new PackageDeclaration (ASTHelper/createNameExpr "whatsys.percolator.test")))
    (ASTHelper/addTypeDeclaration cu class-decl)
    (.toString cu)
    ))

(defmacro vomit-class-decl [modifiers class-name & body-decls]
  `(println (wrap-a-class-kluge ~(interpret-class-decl modifiers class-name body-decls))))

; Action!
(vomit-class-decl
  #{:public :final} "MySickClass"
     'decl-method
       #{:private :synchronized}
       java.lang.String<x>
       "headbang"
       [(int x) (int y) (String args ...)]
         ( 'if ( '== 2 3 ) (('return)) (('return false)))
         ( 'for ( 'local #{} int (x 0) ) ( '< x 5 ) ( '++ x )
           ( '. System/out println x ))
         ( '< 1 2 )
         ( 'return (+ 3 2))
         ( 'xor 1 2 )
         ( '+= x 3 )
         ( '< x nil )
         ( '== x false )
         ( '== x \f )
         ( '== x 3.1415 )
         ( 'super )
         ( 'return ( 'this ))
         ( '* ('- 6 7) 4)      ; holy fuck japaparser does not preserve order of operations? LAME
         ( '- 6 ('* 7 4))      ; holy fuck japaparser does not preserve order of operations? LAME
         ( 'new Shit<int> ( 'new Ass 5 ) )
         ( 'local #{:volatile} int (x 3) (y 4) (z))
         ( 'do-while ( '< x 3 )
           ( '. System/out println "doin stuff" )
           ( 'if ('== ( '. this getStatus ) "bad") (('break))))
         ( 'continue )
         ( 'foreach ( 'local #{} int (foo) )
           ( '. this someCollection )
           ( '. foo someOperation )
           )
         ( 'switch ( '. foo someOperation )
             ( 3 ( '. dong someReaction ) )
             ( 'default ( '. dong someShit ))
             )
         ( 'throw ('new Fuckballs 9) )
    )
