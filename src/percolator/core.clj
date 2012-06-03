(ns percolator.core
  (:use [clojure.contrib.string :as string]))

(import '(japa.parser ASTHelper)) ; we'll help ourselves but VOID_TYPE is used

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
                               VariableDeclarator
    ;public VariableDeclarator(VariableDeclaratorId id, Expression init) {
                               VariableDeclaratorId
    ; takes string name
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
                               VariableDeclarationExpr          ; TODO needs types
    ;public VariableDeclarationExpr(int modifiers, Type type, List<VariableDeclarator> vars) {
                               ))

; Types
(import '( japa.parser.ast.type ClassOrInterfaceType 
    ;public ClassOrInterfaceType(ClassOrInterfaceType scope, String name) {
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
    (.setPackage cu (new PackageDeclaration (ASTHelper/createNameExpr "whatsys.percolator.test")))
    (ASTHelper/addTypeDeclaration cu atype)
    (.setModifiers amethod (ModifierSet/addModifier (.getModifiers amethod) (ModifierSet/STATIC)))
    (ASTHelper/addMember atype amethod)
    (.setVarArgs param true)
    (ASTHelper/addParameter amethod param)
    (.setBody amethod block-stmt)
    (.toString cu)
    ))

(defn interpret-type [form]
  `(new ClassOrInterfaceType
        nil   ; FIXME TODO this argument, scope, will be needed for instantiating outer classes from inner classes
        ;~(apply str (nthrest (.toString form) 1)))
        ~(.toString form)))

; TODO smarter type inference
; and override syntax
(defmulti interpret-expression class)

; clojure literals to java literals
(defmethod interpret-expression java.lang.String    [string] `(new StringLiteralExpr  ~string))
(defmethod interpret-expression java.lang.Long      [long]   `(new LongLiteralExpr    ~(.toString long)))
(defmethod interpret-expression java.lang.Boolean   [bool]   `(new BooleanLiteralExpr ~bool))
(defmethod interpret-expression java.lang.Character [char]   `(new CharLiteralExpr    ~(.toString char)))
(defmethod interpret-expression java.lang.Double    [double] `(new DoubleLiteralExpr  ~(.toString double)))
(defmethod interpret-expression nil                 [& a]    `(new NullLiteralExpr ))

; clojure symbols as a shorthand for NameExpr and FieldAccessExpr-of-NameExpr
; i.e. System or System/out
(defmethod interpret-expression clojure.lang.Symbol [symbol]
  (if (re-find #"^\w*\/\w*$" (.toString symbol))
    (let [ name-and-field (string/split #"/" (.toString symbol)) ]
      `(new FieldAccessExpr (new NameExpr ~(first name-and-field)) ~(last name-and-field))
      )
    `(new NameExpr ~(.toString symbol))
    ))


(defn eval-and-interpret [list] (interpret-expression (eval list)))

(comment for debugging
(defn eval-and-interpret [list]
  (interpret-expression "DIDNT MATCH SYNTAX"))
  )

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

; has to be distinct from the above because the names collide
; e.g. '- can be a unary negation or a subtraction
(def japaparser-operator-type-unary
  { '(quote +           ) 'UnaryExpr$Operator/positive
    '(quote -           ) 'UnaryExpr$Operator/negative
    '(quote ++          ) 'UnaryExpr$Operator/preIncrement
    '(quote --          ) 'UnaryExpr$Operator/preDecrement
    '(quote !           ) 'UnaryExpr$Operator/not
    '(quote bit-inverse ) 'UnaryExpr$Operator/inverse          ; reader pissed about '~
    '(quote +++         ) 'UnaryExpr$Operator/posIncrement     ; order dictates difference in java ... not going to go there
    '(quote ---         ) 'UnaryExpr$Operator/posDecrement
  })

(defn interpret-expression-new [form]
  (let [ type-name (nth form 1)
         arguments (map interpret-expression (nthrest form 2))
       ]

    `(new ObjectCreationExpr
        nil  ; FIXME this argument, scope, can be used for instantiating outer classes from inner classes
        ~(interpret-type (nth form 1))
        (doto (new java.util.ArrayList)
          ~@(map #( cons '.add [%1] ) arguments)
          ))))

(defn interpret-expression-this [form] `(new ThisExpr))

(defn interpret-expression-super [form]
  (if (= 2 (count form))
    `(new SuperExpr ~(interpret-expression (nth form 1))) ; e.g. SomeClass.this (use case: this in anonymous class referring to the enclosing class)
    `(new SuperExpr)
    ))

(defn interpret-expression-unary-operation [expr]
  (let [ operator (japaparser-operator-type-unary (nth expr 0))
         operand  (interpret-expression     (nth expr 1))
       ]
    `(new UnaryExpr ~operand ~operator)
    ))

; there's a slight ambiguity problem with + and -
; they can be unary or binary
; this function must decide which based on how many expressions it is given
(defn interpret-expression-ambiguous-binary-or-unary-operation [expr]
  (if (= 3 (count expr)) ; if it's a binary op
    (let [ operator  (japaparser-operator-type (nth expr 0))
           operand-l (interpret-expression     (nth expr 1))
           operand-r (interpret-expression     (nth expr 2))
         ]
      `(new BinaryExpr ~operand-l ~operand-r ~operator)
      )
    ; otherwise it's unary
    (let [ operator  (japaparser-operator-type-unary (nth expr 0))
           operand   (interpret-expression           (nth expr 1))
         ]
      `(new UnaryExpr ~operand ~operator)
      )))

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

(def modifiers-keywords
  { :public         ModifierSet/PUBLIC
    :private        ModifierSet/PRIVATE
    :protected      ModifierSet/PROTECTED
    :static         ModifierSet/STATIC
    :final          ModifierSet/FINAL
    :synchronized   ModifierSet/SYNCHRONIZED
    :volatile       ModifierSet/VOLATILE
    :transient      ModifierSet/TRANSIENT
    :native         ModifierSet/NATIVE
    :abstract       ModifierSet/ABSTRACT
    :strictfp       ModifierSet/STRICTFP
   })

(defn interpret-modifiers [form]
  (reduce bit-or 0 (map modifiers-keywords form)))

(defn interpret-declarator [form]
  (let [ name        (.toString (nth form 0))
         initializer (first (nthrest form 1))
       ]
    `(new VariableDeclarator
       (new VariableDeclaratorId ~name)
       ~(if initializer (interpret-expression initializer))
        )))

(defn interpret-expression-variable-declaration [form]
  (let [ modifiers (interpret-modifiers (nth form 1))
         java-type (interpret-type (nth form 2))
         declarators (map interpret-declarator (nthrest form 3))
         ]
  `(new VariableDeclarationExpr
     ~modifiers
     ~java-type 
     (doto (new java.util.ArrayList)
       ~@(map #( cons '.add [%1] ) declarators)
       )
        )
    )
  )
    ;public VariableDeclarationExpr(int modifiers, Type type, List<VariableDeclarator> vars) {

(interpret-expression-variable-declaration
  '( 'local #{} Sometype (x) ))



; this is kinda the central point of definition of the syntax of this library
; it associates first-elements of clojure forms
; with functions which interpret those forms as various Java-AST-constructing macros
; eval-and-interpret is the default...
; the idea behind that is that it leaves open the possibility of clojure runtime code
; calculating constants that end up as java literals
; or other fun java-compile-time logic
; the thing returned by the arbitrary clojure code you stuff in there
; could be a literal but it could also be any other clojure form that is a
; valid percolator syntax
; ... which is badass
(defmethod interpret-expression clojure.lang.IPersistentList [list]
  (({ '(quote .    ) interpret-expression-method-call
      '(quote ==   ) interpret-expression-binary-operation
      '(quote !=   ) interpret-expression-binary-operation
      '(quote <=   ) interpret-expression-binary-operation
      '(quote >=   ) interpret-expression-binary-operation
      '(quote <    ) interpret-expression-binary-operation
      '(quote >    ) interpret-expression-binary-operation
      '(quote <<   ) interpret-expression-binary-operation
      '(quote >>   ) interpret-expression-binary-operation
      '(quote >>>  ) interpret-expression-binary-operation
      '(quote +    ) interpret-expression-ambiguous-binary-or-unary-operation
      '(quote -    ) interpret-expression-ambiguous-binary-or-unary-operation
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
     ; unary operation expressions, except for those that are ambiguous (see above, they are + and -)
      '(quote ++          ) interpret-expression-unary-operation
      '(quote --          ) interpret-expression-unary-operation
      '(quote !           ) interpret-expression-unary-operation
      '(quote bit-inverse ) interpret-expression-unary-operation
      '(quote +++         ) interpret-expression-unary-operation
      '(quote ---         ) interpret-expression-unary-operation
     ; miscellaneous expression
      '(quote super) interpret-expression-super
      '(quote this)  interpret-expression-this
      '(quote new)   interpret-expression-new
     ; local variable declaration
      '(quote local ) interpret-expression-variable-declaration
    } (first list)
    eval-and-interpret ; default
    ) list))

(interpret-expression 'Balls/cow)
(interpret-expression 23)
(interpret-expression
'('. cow moo "holy fuckin shit")
  )
(interpret-expression
  '( '. System/out println "yes" ))
(interpret-expression '(+ 2 3))
(interpret-expression '( '<= 1 2 ))


;; interpret-expression should be a multimethod
;; if it's any clojure literal turn it into a java literal expression object expression (yes that makes sense, not a mistype!)
;; if it's clojure code eval it and return the result of interpret-expression(eval(that))?
;; if it's a list beginning with . then it's a method call
;; then just need a way to express java expressions which aren't function calls
;; or field access expressions or simple name expressions
;; that leaves arithmetic, control flow, class and interface definitions,
;; function definitions, type declarations, and probably other stuff I am
;; forgetting...

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

(defn interpret-block [stmt-list]
  `(doto (new BlockStmt)
    (.setStmts (doto (new java.util.ArrayList)
    ~@(
       map #(cons '.add [%1]) (map interpret-statement stmt-list)
       )
      ))
     ) )

(defmacro vomit-block [& stmt-list]
  `(println (spit-it-out ~(interpret-block stmt-list))))

; not quite satisfied with the look of the above but it works

; Action!
(vomit-block
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
  ( 'new Shit<Ass> ( 'new Ass 5 ) )
  ( 'local #{:volatile} Sometype (x 3) (y 4) (z))
  )
