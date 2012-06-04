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
                               TypeDeclarationStmt                   ; TODO <---------------------------------
                               WhileStmt                             ; done
                               )
(japa.parser.ast.expr AnnotationExpr
                               ArrayAccessExpr                  ; TODO
                               ArrayCreationExpr                ; TODO
                               ArrayInitializerExpr             ; TODO
                               AssignExpr                       ; done
                               AssignExpr$Operator              ; done
                               BinaryExpr                       ; done
                               BinaryExpr$Operator              ; done
                               BooleanLiteralExpr               ; done
                               CastExpr                         ; TODO think of a syntax
                               CharLiteralExpr                  ; done
                               ClassExpr                        ; wtf
                               ConditionalExpr                  ; aka ternary TODO
                               DoubleLiteralExpr                ; done
                               EnclosedExpr                     ; wtf
                               FieldAccessExpr                  ; done-ish with special /-in-a-symbol syntax (only possible if target is a NameExpr)
                               InstanceOfExpr                   ; TODO
                               IntegerLiteralExpr               ; not possible everything is a long, no biggie
                               IntegerLiteralMinValueExpr       ; wtf
                               LiteralExpr                      ; abstract
                               LongLiteralExpr                  ; done-ish ... better type inference from clojure primitives
                               LongLiteralMinValueExpr          ; wtf
                               MarkerAnnotationExpr             ; wtf
                               MethodCallExpr                   ; done (anything missing?)
                               NameExpr                         ; done
                               NormalAnnotationExpr             ; wtf
                               NullLiteralExpr                  ; done
                               ObjectCreationExpr               ; done-ish, doesn't support outer/inner classes
                               QualifiedNameExpr                ; done-ish with the /-in-a-symbol syntax
                               SingleMemberAnnotationExpr       ; wtf
                               StringLiteralExpr                ; done
                               SuperExpr                        ; done
                               ThisExpr                         ; done
                               UnaryExpr                        ; done
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

(defn interpret-parameter [param-type param-name & array-count-or-varargs]
  (let [ param-type             (reference-type (interpret-type param-type))
         param-name             (.toString param-name)
         array-count-or-varargs (first array-count-or-varargs)
         param-construction     `( ASTHelper/createParameter ~param-type ~param-name )
       ]
    (case array-count-or-varargs
      ...  `(doto ~param-construction (.setVarArgs true))
      nil  param-construction)))

(defn interpret-body-decl-method [modifiers return-type method-name param-list & body]
  `(doto
     (new MethodDeclaration
          ~(interpret-modifiers modifiers)
          ~(interpret-type return-type)
          ~(.toString method-name)
          [ ~@(map #(apply interpret-parameter %1) param-list) ] )
     (.setBody ~(interpret-block body))))

(defn interpret-body-decl-field [modifiers java-type & declarators]
  `(new FieldDeclaration
        ~(interpret-modifiers modifiers)
        ~(interpret-type java-type)
        [ ~@(map #(apply interpret-declarator %1) declarators) ] )) 

(defn interpret-body-decl [form]
  (let [ interpreter (body-decl-interpreters (first form))
         arguments   (drop 1 form) ]
    (if interpreter
      (apply interpreter arguments)
      (interpret-body-decl (eval form)))))

(defn is-class-modifier-option [body-decl]
  ( #{ '(quote implements) '(quote extends) } (first body-decl)))

(defn snip-class-modifier-options-from-body-decls [body-decls]
  {
    :class-modifier-options
    (keep #( when (is-class-modifier-option %1) %1 ) body-decls)
    :body-decls
    (keep #( when-not (is-class-modifier-option %1) %1 ) body-decls)
   })

(defn interpret-class-modifier-option [form]
  (when form (map interpret-type (nthrest form 1))))

(defn first-form-that-looks-like [first-form forms]
  (some #( when ( = (first %1) first-form ) %1 ) forms))

; class modifier options are forms that live in the class body forms
; along with body-decls
; but they aren't body-decls at all, they affect the enclosing class
; used for implements and extends at the moment
(defn interpret-class-modifier-options [class-modifier-options]
  { :implements-list
      (interpret-class-modifier-option (first-form-that-looks-like '(quote implements) class-modifier-options))
    :extends-list
      (interpret-class-modifier-option (first-form-that-looks-like '(quote extends) class-modifier-options))
   })

(defn interpret-class-decl [modifiers class-name & body-decls]
  (let [ { :keys [class-modifier-options body-decls]} (snip-class-modifier-options-from-body-decls body-decls)
         { :keys [implements-list extends-list]} (interpret-class-modifier-options class-modifier-options)
        ]
    `( new ClassOrInterfaceDeclaration
          nil ; javadoc
          ~(interpret-modifiers modifiers)
          nil ; annotations
          false ; isInterface
          ~(.toString class-name)
          nil ; list of TypeParameter
          [ ~@extends-list ]
          [ ~@implements-list ]
          [ ~@( map interpret-body-decl body-decls ) ] )))

(def body-decl-interpreters
  { '(quote method) interpret-body-decl-method
    '(quote field)  interpret-body-decl-field
    '(quote class)  interpret-class-decl
    })

; keep in mind that all body declarations share 2 things in common
; they can have javadocs and they can have annotations
; make sure that's generic in syntax and implementation
