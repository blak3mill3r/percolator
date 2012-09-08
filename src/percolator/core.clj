(ns percolator.core)
(declare compilation-units-in-namespace expression-interpreter-for-form interpret-expression-again-or-identity interpret-statement-again-or-identity expression-interpreters interpret-declarator eval-and-interpret interpret-expression-binary-operation split-arguments-and-body-decls interpret-expression-this interpret-expression-variable-declaration interpret-expression-new interpret-expression-super japaparser-operator-type-unary interpret-expression-ambiguous-binary-or-unary-operation interpret-expression-assignment-operation japaparser-operator-constant interpret-expression interpret-expression-unary-operation interpret-expression-method-call
primitive-type interpret-type reference-type
first-form-that-looks-like interpret-modifiers modifiers-keywords partition-by-starts-with
interpret-statement-do-while interpret-block interpret-statement-throw interpret-statement-for interpret-statement-foreach interpret-statement interpret-switch-entry-statement interpret-statement-if interpret-statement-while interpret-statement-switch interpret-statement-break interpret-statement-return statement-interpreters interpret-statement-continue
is-class-modifier-option interpret-class-modifier-option interpret-body-decl-ctor body-decl-interpreters interpret-body-decl-method interpret-body-decl-class interpret-body-decl-interface interpret-body-decl interpret-body-decl-field interpret-parameter snip-class-modifier-options-from-body-decls interpret-class-modifier-options
vomit-class-decl return-false add-two-to-s compilation-unit definterpreter interpreter reset-scope interpret-in-scope)

(ns percolator.core
  (:use [clojure.java.io :only (file writer)])
  (:require [clojure.string :as string ])
  (:import
    (japa.parser.ast.body AnnotationDeclaration 
                          AnnotationMemberDeclaration 
                          JavadocComment
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
(japa.parser.ast BlockComment
                 CompilationUnit
                 PackageDeclaration
                 ImportDeclaration)
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
                               TypeDeclarationStmt                   ; done
                               WhileStmt                             ; done
                               )
(japa.parser.ast.expr          AnnotationExpr
                               MemberValuePair                  ; part of an annotation
                               NormalAnnotationExpr             ; an annotation with name/value pairs
                               MarkerAnnotationExpr             ; an annotation with values
                               SingleMemberAnnotationExpr       ; an an annotation with 1 'value' value
                               ArrayAccessExpr                  ; an expression accessing an element of an array
                               ArrayCreationExpr                ; ? is this new[] ?
                               ArrayInitializerExpr             ; ? TODO look into this
                               AssignExpr                       ; done
                               AssignExpr$Operator              ; done
                               BinaryExpr                       ; done
                               BinaryExpr$Operator              ; done
                               BooleanLiteralExpr               ; done
                               CastExpr                         ; TODO explicit casts
                               CharLiteralExpr                  ; done
                               ClassExpr                        ; done
                               ConditionalExpr                  ; aka ternary TODO do if statement first
                               DoubleLiteralExpr                ; done
                               EnclosedExpr                     ; wtf
                               FieldAccessExpr                  ; done-ish with special /-in-a-symbol syntax (only possible if target is a NameExpr)
                               InstanceOfExpr                   ; TODO easy
                               IntegerLiteralExpr               ; not possible everything is a long, no biggie
                               IntegerLiteralMinValueExpr       ; wtf
                               LiteralExpr                      ; abstract
                               LongLiteralExpr                  ; done-ish ... better type inference from clojure primitives
                               LongLiteralMinValueExpr          ; wtf
                               MethodCallExpr                   ; done (anything missing?)
                               NameExpr                         ; done
                               NullLiteralExpr                  ; done
                               ObjectCreationExpr               ; done-ish, doesn't support outer/inner classes
                               QualifiedNameExpr                ; done-ish with the /-in-a-symbol syntax
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
                       WildcardType )))

(load "extension" "interpreter" "util" "expression" "declaration" "japaparser" "statement" "type")

; a mapping of package/class names to clojure vars by a convention
; which is used by percolator to identify compilation units in a namespace
(defn cu-auto-name [package-name class-name & suffix]
    (string/replace (string/join (map #(.toString %) ["cu-" package-name "--" class-name (string/join suffix)]))
                  #"\."
                  "-"))

; the class name is the third form in the class-decl form
(defn class-name-of [form] (nth form 2))

(defmacro compilation-unit [metadata package-decl import-decls class-decl]
  (let [ var-name          (symbol (cu-auto-name package-decl (class-name-of class-decl))) ]
   `(def ~var-name
      { :ast 
        (new CompilationUnit
         ~(interpret-package-declaration package-decl)
          [~@(map interpret-import-decl import-decls)]
          [~class-decl]
          [] ;comments FIXME add support
          )
        :metadata ~metadata
       })))


(defmacro class-decl [& args]
  (apply interpret-body-decl-class args))

(defmacro interface-decl [& args]
  (apply interpret-body-decl-interface args))

; by convention they are public vars beginning with cu-
(defn compilation-units-in-namespace [a-namespace]
  (filter
    #(re-find #"^cu-" (.toString %))
     (keys (ns-publics a-namespace))))

(defn source-path-for-unit [cu]
  (string/replace 
    (.toString ( .getName (.getPackage cu)))
    "." "/"))

(defn java-file-name-for-unit [cu]
  (.getName (first (.getTypes cu)))) ; FIXME this relies on there being only 1 type in the cu FIXME retarded will break

(defn relative-path-for-cu [cu]
  (string/join [
    (string/join "/" [ ( source-path-for-unit cu ) ( java-file-name-for-unit cu ) ])
    ".java"
  ]))



; write a single percolator cu to the given path
(defn write-cu-to-path [cu path]
  (let [{:keys [ast metadata]} cu]
    (let [{:keys [postprocessors] :or []} metadata ]
      (let [ relative-path ( relative-path-for-cu ast )
             full-path (string/join "/" [path relative-path] )
             postprocessor (or (first postprocessors) identity) ; FIXME limited to 1 postprocessor
             java-source-string ( postprocessor (.toString ast)  )
            ]
        (do
          (println "writing," relative-path  "metadata is " metadata " and postprocessors " postprocessors " the postprocessor is  " postprocessor)
          (with-open [w (writer (file full-path))]
            (binding [*out* w]
              (print java-source-string ))
            full-path)))))) ; return the path I guess, instead of the result of print?

; write .java files for all the percolator compilation units defined in cu-namespace
(defn write-all-cus-to-path [cu-namespace path]
  (doseq [cu (compilation-units-in-namespace cu-namespace) ]
    (write-cu-to-path @(ns-resolve cu-namespace cu ) path)))
