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
                               AssignExpr$Operator
                               BinaryExpr                      ; done
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

(defmethod interpret-expression nil [a]
  `( new StringLiteralExpr (.toString a) )
  )

(defn eval-and-interpret [list]
  (interpret-expression (eval list)))

(comment for debugging
(defn eval-and-interpret [list]
  (interpret-expression "DIDNT MATCH SYNTAX"))
  )

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
  (({ '(quote ==   ) interpret-expression-binary-operation
      '(quote !=   ) interpret-expression-binary-operation
      '(quote <=   ) interpret-expression-binary-operation
      '(quote >=   ) interpret-expression-binary-operation
      '(quote <    ) interpret-expression-binary-operation
      '(quote >    ) interpret-expression-binary-operation
      '(quote <<   ) interpret-expression-binary-operation
      '(quote >>   ) interpret-expression-binary-operation
      '(quote >>>  ) interpret-expression-binary-operation
      '(quote +    ) interpret-expression-binary-operation
      '(quote -    ) interpret-expression-binary-operation
      '(quote *    ) interpret-expression-binary-operation
      '(quote /    ) interpret-expression-binary-operation
      '(quote %    ) interpret-expression-binary-operation
      '(quote xor  ) interpret-expression-binary-operation ; xor has to be special because '^ pisses the reader off (room for improvement)
      '(quote ||   ) interpret-expression-binary-operation
      '(quote &&   ) interpret-expression-binary-operation
      '(quote |    ) interpret-expression-binary-operation
      '(quote &    ) interpret-expression-binary-operation
     ; assignment expressions
      '(quote =    ) interpret-expression-assignment-operation
      '(quote +=   ) interpret-expression-assignment-operation
      '(quote -=   ) interpret-expression-assignment-operation
      '(quote *=   ) interpret-expression-assignment-operation
      '(quote slash=   ) interpret-expression-assignment-operation
      '(quote &=   ) interpret-expression-assignment-operation
      '(quote |=   ) interpret-expression-assignment-operation
      '(quote xor= ) interpret-expression-assignment-operation
      '(quote %=   ) interpret-expression-assignment-operation
      '(quote <<=  ) interpret-expression-assignment-operation
      '(quote >>=  ) interpret-expression-assignment-operation
      '(quote >>>= ) interpret-expression-assignment-operation
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
; symbol aliases
; which resolves to an Operator constant from japaparser
(def japaparser-operator-type
  { '(quote ==   ) 'BinaryExpr$Operator/equals
    '(quote !=   ) 'BinaryExpr$Operator/notEquals
    '(quote <=   ) 'BinaryExpr$Operator/lessEquals
    '(quote >=   ) 'BinaryExpr$Operator/greaterEquals
    '(quote <    ) 'BinaryExpr$Operator/less
    '(quote >    ) 'BinaryExpr$Operator/greater
    '(quote <<   ) 'BinaryExpr$Operator/lShift
    '(quote >>   ) 'BinaryExpr$Operator/rSignedShift
    '(quote >>>  ) 'BinaryExpr$Operator/rUnsignedShift
    '(quote +    ) 'BinaryExpr$Operator/plus
    '(quote -    ) 'BinaryExpr$Operator/minus
    '(quote *    ) 'BinaryExpr$Operator/times
    '(quote /    ) 'BinaryExpr$Operator/divide
    '(quote %    ) 'BinaryExpr$Operator/remainder
    '(quote xor  ) 'BinaryExpr$Operator/xor
    '(quote ||   ) 'BinaryExpr$Operator/or
    '(quote &&   ) 'BinaryExpr$Operator/and
    '(quote |    ) 'BinaryExpr$Operator/binOr
    '(quote &    ) 'BinaryExpr$Operator/binAnd
   ; assignment operators
    '(quote =    ) 'AssignExpr$Operator/assign
    '(quote +=   ) 'AssignExpr$Operator/plus
    '(quote -=   ) 'AssignExpr$Operator/minus
    '(quote *=   ) 'AssignExpr$Operator/star
    '(quote slash= ) 'AssignExpr$Operator/slash  ; irritating, reader pissed at '\= (  but not at '\  )
    '(quote &=   ) 'AssignExpr$Operator/and
    '(quote |=   ) 'AssignExpr$Operator/or
    '(quote xor= ) 'AssignExpr$Operator/xor
    '(quote %=   ) 'AssignExpr$Operator/rem
    '(quote <<=  ) 'AssignExpr$Operator/lShift
    '(quote >>=  ) 'AssignExpr$Operator/rSignedShift
    '(quote >>>= ) 'AssignExpr$Operator/rUnsignedShift
  })

(defn interpret-expression-binary-operation [expr]
  (let [ operator  (japaparser-operator-type (nth expr 0))
         operand-l (interpret-expression     (nth expr 1))
         operand-r (interpret-expression     (nth expr 2))
        ]
    `(new BinaryExpr ~operand-l ~operand-r ~operator)
    ))

(defn interpret-expression-assignment-operation [expr]
  (let [ operator  (japaparser-operator-type (nth expr 0))
         target    (interpret-expression     (nth expr 1))
         value     (interpret-expression     (nth expr 2))
        ]
    `(new AssignExpr ~target ~value ~operator)
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

(defmulti interpret-statement first :default :default)
(defmethod interpret-statement '(quote return) [expression]
  (if (= (count expression) 2)
    `(new japa.parser.ast.stmt.ReturnStmt
       ~(interpret-expression (nth expression 1)))
    `(new japa.parser.ast.stmt.ReturnStmt)
    ))

(defmethod interpret-statement :default [expr]
  `(new ExpressionStmt ~(interpret-expression expr)))

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
; not quite satisfied with the look of the above but it works

; Action!
(vomit-block
  ( '< 1 2 )
  ( 'return (+ 3 2))
  ( 'xor 1 2 )
  ( '+= x 3 )
  ( '< x 1 )
  
  )
