(ns clojurejavacodegen.core
  (:use [clojure.contrib.string :as string]))

;(import '(japa.parser ASTHelper)) ; we'll help ourselves

; Various other bits of AST
(import '(japa.parser.ast CompilationUnit))
(import '(japa.parser.ast PackageDeclaration))

; Declarations and such
(import '(japa.parser.ast.body AnnotationDeclaration 
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
                               ))

; Statements
(import '(japa.parser.ast.stmt AssertStmt 
                               BlockStmt 
                               BreakStmt 
                               ContinueStmt 
                               DoStmt 
                               EmptyStmt 
                               ExplicitConstructorInvocationStmt 
                               ExpressionStmt
                               ForeachStmt 
                               ForStmt 
                               IfStmt 
                               LabeledStmt 
                               ReturnStmt 
                               SwitchEntryStmt 
                               SwitchStmt 
                               SynchronizedStmt 
                               ThrowStmt 
                               TryStmt 
                               TypeDeclarationStmt 
                               WhileStmt
                               ))

; Expressions
(import '(japa.parser.ast.expr AnnotationExpr
                               ArrayAccessExpr 
                               ArrayCreationExpr 
                               ArrayInitializerExpr 
                               AssignExpr 
                               BinaryExpr 
                               BinaryExpr$Operator
                               BooleanLiteralExpr
                               CastExpr 
                               CharLiteralExpr 
                               ClassExpr 
                               ConditionalExpr 
                               DoubleLiteralExpr 
                               EnclosedExpr 
                               FieldAccessExpr 
                               InstanceOfExpr 
                               IntegerLiteralExpr 
                               IntegerLiteralMinValueExpr 
                               LiteralExpr 
                               LongLiteralExpr 
                               LongLiteralMinValueExpr 
                               MarkerAnnotationExpr 
                               MethodCallExpr 
                               NameExpr 
                               NormalAnnotationExpr 
                               NullLiteralExpr 
                               ObjectCreationExpr 
                               QualifiedNameExpr 
                               SingleMemberAnnotationExpr 
                               StringLiteralExpr 
                               SuperExpr 
                               ThisExpr
                               UnaryExpr
                               VariableDeclarationExpr
                               ))

; Types
(import '( japa.parser.ast.type ClassOrInterfaceType 
                                PrimitiveType         ; a degrading term
                                ReferenceType 
                                Type
                                VoidType 
                                WildcardType ))

(defn spit-it-out [block-stmt]
  (let [ cu                (new CompilationUnit)
         atype             (new ClassOrInterfaceDeclaration (ModifierSet/PUBLIC) false "GeneratedClass")
         amethod           (new MethodDeclaration (ModifierSet/PUBLIC) (ASTHelper/VOID_TYPE) "main")
         param             (ASTHelper/createParameter (ASTHelper/createReferenceType "String" 0) "args")
       ]
    (.setPackage cu (new PackageDeclaration (ASTHelper/createNameExpr "whatsys.clojurejavacodegen.test")))
    (ASTHelper/addTypeDeclaration cu atype)
    (.setModifiers amethod (ModifierSet/addModifier (.getModifiers amethod) (ModifierSet/STATIC)))
    (ASTHelper/addMember atype amethod)
    (.setVarArgs param true)
    (ASTHelper/addParameter amethod param)
    (.setBody amethod block-stmt)
    (.toString cu)
    ))

;(defn kkk [expr]
;  (let [ ablock            (new BlockStmt)
;         class-name-expr   (new NameExpr "System")
;         field             (new FieldAccessExpr class-name-expr "out")
;         call              (new MethodCallExpr field "println")
;       ]
;    (ASTHelper/addArgument call (new StringLiteralExpr "HELLO THERE"))
;    (ASTHelper/addStmt ablock call)
;    (spit-it-out ablock)))
;
;(defn kkk [expr]
;  (spit-it-out
;    (doto (new BlockStmt)
;      (.setStmts (doto (new java.util.ArrayList)
;                   (.add (new ExpressionStmt 
;                           (new MethodCallExpr
;                             (eval (name-and-field-symbol-to-java-expr 'Systom/fuck))
;                             "println"
;                           )
;                              )))))))

;; so ... this is limited to string literal arguments to functions
;; which is less than ideal
;; clojure literals -> java literals
;; clojure lists beginning with . -> java expressions
;;
; TODO smarter type inference
; and override syntax
(defmulti interpret-expression class)

(defmethod interpret-expression java.lang.String [string]
  `(new StringLiteralExpr ~string))


(defmethod interpret-expression java.lang.Long [long]
  `(new LongLiteralExpr (.toString ~long)))

(defn eval-and-interpret [list]
  (interpret-expression (eval list)))

; this is kinda the central point of definition of the syntax of this library
; it associates first-elements of clojure forms
; with functions which interpret those forms as various Java-AST-constructing macros
; eval-and-interpret is the default...
; the idea behind that is that it leaves open the possibility of clojure runtime code
; calculating constants that end up as java literals
; or other fun java-compile-time logic
; the thing returned by the arbitrary clojure code you stuff in there
; could be a literal but it could also be any other clojure form that is a
; valid clojurejavacodegen syntax
; ... which is badass
(defmethod interpret-expression clojure.lang.IPersistentList [list]
  (({ '.  interpret-expression-method-call
      :==  interpret-expression-binary-operation
      :!=  interpret-expression-binary-operation
      :<=  interpret-expression-binary-operation
      :>=  interpret-expression-binary-operation
      :<   interpret-expression-binary-operation
      :>   interpret-expression-binary-operation
      :<<  interpret-expression-binary-operation
      :>>  interpret-expression-binary-operation
      :>>> interpret-expression-binary-operation
      :+   interpret-expression-binary-operation
      :-   interpret-expression-binary-operation
      :*   interpret-expression-binary-operation
      (keyword "/") interpret-expression-binary-operation
      (keyword "%") interpret-expression-binary-operation
    } (first list)
    eval-and-interpret ; default
    ) list))

(defmethod interpret-expression clojure.lang.Symbol [symbol]
  (if (re-find #"^\w*\/\w*$" (.toString symbol))
    (let [ name-and-field (string/split #"/" (.toString symbol)) ]
      `(new FieldAccessExpr (new NameExpr ~(first name-and-field)) ~(last name-and-field))
      )
    `(new NameExpr ~(.toString symbol))
    ))

(def k (keyword "%"))
; given a keyword, returns a symbol
; which resolves to an Operator constant from japaparser
(def japaparser-operator-type
  { :==  'BinaryExpr$Operator/equals
    :!=  'BinaryExpr$Operator/notEquals
    :<=  'BinaryExpr$Operator/lessEquals
    :>=  'BinaryExpr$Operator/greaterEquals
    :<   'BinaryExpr$Operator/less
    :>   'BinaryExpr$Operator/greater
    :<<  'BinaryExpr$Operator/lShift
    :>>  'BinaryExpr$Operator/rSignedShift
    :>>> 'BinaryExpr$Operator/rUnsignedShift
    :+   'BinaryExpr$Operator/plus
    :-   'BinaryExpr$Operator/minus
    :*   'BinaryExpr$Operator/times
    (keyword "/")   'BinaryExpr$Operator/divide
    (keyword "%")  'BinaryExpr$Operator/remainder
  })

(defn interpret-expression-binary-operation [expr]
  (let [ operator  (japaparser-operator-type (nth expr 0))
         operand-l (interpret-expression     (nth expr 1))
         operand-r (interpret-expression     (nth expr 2))
        ]

    `(new BinaryExpr ~operand-l ~operand-r ~operator)
    ))


(defn interpret-expression-method-call [expr]
  (let [ target        (nth expr 1)
         function-name (nth expr 2)
         arguments     (map interpret-expression (nthrest expr 3))
       ]
    `(doto
       (new MethodCallExpr
         ~(interpret-expression target)
         ~(.toString function-name)
         )
       (.setArgs (doto (new java.util.ArrayList)
;                   ~@(map #( .add (literal-string %) ) arguments)
                   ~@(map #( cons '.add [%1] ) arguments)
                   ))
       )
    ))

(interpret-expression 'Balls/cow)
(interpret-expression 'cow)
(interpret-expression 23)
(interpret-expression
'(. cow moo "holy fuckin shit" 3)
  )
(interpret-expression '(+ 2 3))
(interpret-expression '( :<= 1 2 ))


;; interpret-expression should be a multimethod
;; if it's any clojure literal turn it into a java literal expression object expression (yes that makes sense, not a mistype!)
;; if it's clojure code eval it and return the result of interpret-expression(eval(that))?
;; if it's a list beginning with . then it's a method call
;; then just need a way to express java expressions which aren't function calls
;; or field access expressions or simple name expressions
;; that leaves arithmetic, control flow, class and interface definitions,
;; function definitions, type declarations, and probably other stuff I am
;; forgetting...

; turn something like 'System/out
; into
; (new FieldAccessExpr (new NameExpr "System") "out")

(defn name-and-field-symbol-to-java-expr [symbol]
  (let [ name-and-field
           (string/split #"/" (.toString symbol))
         name
           (first name-and-field)
         field
           (last name-and-field)
        ]
    `(new FieldAccessExpr (new NameExpr ~name) ~field)
    ))

(name-and-field-symbol-to-java-expr 'System/out)


;name-and-field-symbol-to-java-expr needs to become a multimethod
;with it's current behavior as the implementation for symbol
;and any code that can represent a java expression object expression
;should be able to be the target of a method call
;
;in fact, I think, name-and-field-symbol-to-java-expr
;should be interpret-expression
;and it's current behavior as an implementation of that for symbol

; this was called method-call-expr
; which is what it is
; but where it's called it should be more generic
; this implementation is just to interpret a statement which happens
; to be a method call expression
; needs to be a multimethod that can dispatch on what kind of statement syntax
; it is

(defmulti interpret-statement first)
(defmethod interpret-statement :return [expression]
  `(new japa.parser.ast.stmt.ReturnStmt
        ~(interpret-expression (nth expression 1))
        ))

(interpret-statement 
           '(:return (. foo amethod ) ) )

(defmethod interpret-statement :default [expr]
  `(new ExpressionStmt ~(interpret-expression expr)))

; must be a cleaner way to do the above
; the map within the splicing unquote thingy
; don't get the backquote syntax here in the anonymous function
; maybe make it a named function but what would be the name (add-each?)

(defmacro vomit-block [& stmt-list]
  `(println (spit-it-out ~(interpret-block stmt-list))))

(defn interpret-block [stmt-list]
  `(doto (new BlockStmt)
    (.setStmts (doto (new java.util.ArrayList)
    ~@(
       map #(cons '.add [%1]) (map interpret-statement stmt-list)
       )
      ))
     ) )

; Action!
(vomit-block
  (. oof amethod 3 5 "woot")
  (. oof amethod 3 5 "dong")
  ( :== 1 2 )
  ( :!= 1 2 )
  ( :<= 1 2 )
  ( :>>> 1 2 )
  (:return (. foo amethod ) )
  
  )

(defmacro add-each [& forms]
  (map #(cons '.add [%1]) forms))

(add-each (new java.lang.String)(new java.lang.String))

  (interpret-statement '(. System/out println "foo"))

(println (kkk

           '(
             .
             (. oof amethod 3 5 "woot")
             println
             "holy fuckin shit"
             3
             )
           
           ))
; that is fucking awesome
; suddenly method call expressions
; can be the targets of method call expressions
; how can this concept be extended
; to express arithmetic expressions
; return expressions
; conditionals
; loops
; assignments
; array accesses
; all the other crap in java expressions
; ?
;
; return expressions:
(:return something)
; if a list begins with return it's a return expression
; seems reasonable...

; conditionals
(if )
; now there's a problem, it conflicts with clojure
; hmmmmmm
; how about using Keywords instead?
(:return something)
(:if condition then-block else-block?)
; that seems solid...

; shove this into the interpret-expression dispatcher
; also, symbols without a / should be ... variable references
(if (re-find #"^\w*\/\w*$" "System/out") :true :false)
(string/split #"/" "System/out")

(ancestors (class '(:fuck :me)))

; or equivalently
(.. System/out (println "foo"))
(macroexpand '(.. System/out (println "foo")))

; learning about macros...
(defmacro arealmacro [fn-name args & body]
  `(defn ~fn-name ~args
     ~@body))

(defn somethinglikeamacro [fn-name args & body]
  `(defn ~fn-name ~args
     ~@body))

(eval
  (somethinglikeamacro 'balls ['dung] '())
  )
(arealmacro balls[dung] ())

(macroexpand '(. System/out println "foo"))
(macroexpand '(.println System/out "foo"))


