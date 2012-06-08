(ns percolator.core)
(declare expression-interpreters interpret-declarator eval-and-interpret interpret-expression-binary-operation split-arguments-and-body-decls interpret-expression-this interpret-expression-variable-declaration interpret-expression-new interpret-expression-super japaparser-operator-type-unary interpret-expression-ambiguous-binary-or-unary-operation interpret-expression-assignment-operation japaparser-operator-constant interpret-expression interpret-expression-unary-operation interpret-expression-method-call)
(declare primitive-type interpret-type reference-type)
(declare first-form-that-looks-like interpret-modifiers modifiers-keywords partition-by-starts-with)
(declare interpret-statement-do-while interpret-block interpret-statement-throw interpret-statement-for interpret-statement-foreach interpret-statement interpret-switch-entry-statement interpret-statement-if interpret-statement-while interpret-statement-switch interpret-statement-break interpret-statement-return statement-interpreters interpret-statement-continue)
(declare is-class-modifier-option interpret-class-modifier-option interpret-body-decl-ctor body-decl-interpreters interpret-body-decl-method interpret-body-decl-class interpret-body-decl interpret-body-decl-field interpret-parameter snip-class-modifier-options-from-body-decls interpret-class-modifier-options)
(declare vomit-class-decl return-false add-two-to-s wrap-a-class-kluge)

(ns percolator.core
  (:require [clojure.contrib.string :as string])
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

(load-file "/home/blake/w/percolator/src/percolator/declaration.clj")
(load-file "/home/blake/w/percolator/src/percolator/expression.clj")
(load-file "/home/blake/w/percolator/src/percolator/japaparser.clj")
(load-file "/home/blake/w/percolator/src/percolator/statement.clj")
(load-file "/home/blake/w/percolator/src/percolator/type.clj")
(load-file "/home/blake/w/percolator/src/percolator/util.clj")

(defn wrap-a-class-kluge [class-decl]
  (let [ cu (new CompilationUnit) ]
    (.setPackage cu (new PackageDeclaration (ASTHelper/createNameExpr "whatsys.percolator.test")))
    (ASTHelper/addTypeDeclaration cu class-decl)
    (.toString cu)
    ))

(defmacro vomit-class-decl [& args]
  `(println (wrap-a-class-kluge ~(apply interpret-body-decl-class args))))

(defn add-two-to-s []
  '( '+= s 2 ))

(defn return-false []
  '( 'return false ))

; Action!
(vomit-class-decl
  #{:public} MyAss ; fixme implements EntryPoint
    ( 'implements Shit Fart )
    ( 'extends Butt )
    ( 'field #{:volatile} int (x) (y) (z))

    ( 'class #{:public} MyInnerFuckingClass
      ( 'field #{:private} String (label) )

      ( 'method
         #{:public}
         int
         status
         []
           ( 'return "BAD" )))
    ( 'ctor
        #{:public}
        "MyAss" ; FIXME shouldn't have to specify class name again here but it's not available ... this gets interpreted before interpret-body-decl-class is finished
        [ (int x) (int y) ]
          ( 'super 42 ) ; FIXME currently this is a super expression on the target 42, it should be an explicit call to super with the parameter 42
          ( '. this doStuff 42 ))
    ( 'method
       #{:private :synchronized}
       java.lang.String<x>
       "headbang"
       [(int x) (int y) (String args ...)]
         ( '+= s 2 )
         ( add-two-to-s )
         ( return-false )
         ( 'break )
         ( 'continue )
         ( 'throw 42 )
         ( '. Something/foo bar baz )
         ( 'if ( '== 2 3 ) (('return)) (('return false)))
         ( '< 1 2 )
         ( 'return (+ 3 2))
         ( 'xor 1 2 )
         ( '+= x 3 )
         ( '< x nil )
         ( '== x false )
         ( '== x \f )
         ( '== x 3.1415 )
         ( 'super )
         ( 'this )
         ( 'new ShittyAss 3 ( 'method #{:public} void toasted [] ) )
         ( 'return ( 'this ))
         ( '* ('- 6 7) 4)      ; holy fuck japaparser does not preserve order of operations? LAME
         ( '- 6 ('* 7 4))      ; holy fuck japaparser does not preserve order of operations? LAME
         ;( 'new Shit<int> ( 'new Ass 5 ) ) ; FIXME I broke this adding anon class body
         ( 'if ('== ( '. this getStatus ) "bad") (('break)))
         ( 'local #{:volatile} int (x 3) (y 4) (z))
         ( 'while ( '< x 3 )
           ( '. System/out println "doin stuff" )
           ( 'if ('== ( '. this getStatus ) "bad") (('break))))
         ( 'continue )
         ( 'do-while ( '< x 3 )
           ( '. System/out println "doin stuff" )
           ( 'if ('== ( '. this getStatus ) "bad") (('break))))
         ( 'continue )
         ( 'for ( #{} int (foo) ) ( '< x 5 ) ( '++ x )
           ( '. System/out println x ))
         ( 'foreach ( #{} int (foo) )
           ( '. this someCollection )
           ( '. foo someOperation )
           )
         ( 'switch ( '. foo someOperation )
             (3 ( 'return 1 ))
             ('default ( 'return 69 ))
             )
         ( 'throw ('new Fuckballs) )
         ;( 'throw ('new Fuckballs 9) ) ; broken by anon class body
        ))
