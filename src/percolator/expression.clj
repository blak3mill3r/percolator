(ns percolator.expression
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
    )
  (:use percolator.type
        percolator.util)
  )

; symbol aliases
; which resolves to an Operator constant from japaparser
(def japaparser-operator-constant
  { '==      'BinaryExpr$Operator/equals
    '!=      'BinaryExpr$Operator/notEquals
    '<=      'BinaryExpr$Operator/lessEquals
    '>=      'BinaryExpr$Operator/greaterEquals
    '<       'BinaryExpr$Operator/less
    '>       'BinaryExpr$Operator/greater
    '<<      'BinaryExpr$Operator/lShift
    '>>      'BinaryExpr$Operator/rSignedShift
    '>>>     'BinaryExpr$Operator/rUnsignedShift
    '+       'BinaryExpr$Operator/plus
    '-       'BinaryExpr$Operator/minus
    '*       'BinaryExpr$Operator/times
    '/       'BinaryExpr$Operator/divide
    '%       'BinaryExpr$Operator/remainder
    'xor     'BinaryExpr$Operator/xor
    '||      'BinaryExpr$Operator/or
    '&&      'BinaryExpr$Operator/and
    '|       'BinaryExpr$Operator/binOr
    '&       'BinaryExpr$Operator/binAnd
   ; assigment
    '=       'AssignExpr$Operator/assign
    '+=      'AssignExpr$Operator/plus
    '-=      'AssignExpr$Operator/minus
    '*=      'AssignExpr$Operator/star
    'slash=  'AssignExpr$Operator/slash  ; irritating, reader pissed at '\= (  but not at '\  )
    '&=      'AssignExpr$Operator/and
    '|=      'AssignExpr$Operator/or
    'xor=    'AssignExpr$Operator/xor
    '%=      'AssignExpr$Operator/rem
    '<<=     'AssignExpr$Operator/lShift
    '>>=     'AssignExpr$Operator/rSignedShift
    '>>>=    'AssignExpr$Operator/rUnsignedShift
  })

; has to be distinct from the above because the names collide
; e.g. '- can be a unary negation or a subtraction
(def japaparser-operator-type-unary
  { '+            'UnaryExpr$Operator/positive
    '-            'UnaryExpr$Operator/negative
    '++           'UnaryExpr$Operator/preIncrement
    '--           'UnaryExpr$Operator/preDecrement
    '!            'UnaryExpr$Operator/not
    'bit-inverse  'UnaryExpr$Operator/inverse          ; reader pissed about '~
    '+++          'UnaryExpr$Operator/posIncrement     ; order dictates difference in java ... not going to go there
    '---          'UnaryExpr$Operator/posDecrement
  })


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

(defn interpret-expression-new [type-name & arguments]
  `(new ObjectCreationExpr
      nil  ; FIXME this argument, scope, can be used for instantiating outer classes from inner classes
      ~(interpret-type type-name)
      [ ~@(map interpret-expression arguments) ]
        ))

(defn interpret-expression-this [] `(new ThisExpr))

(defn interpret-expression-super [& target-class]
  (if (first target-class)
    `(new SuperExpr ~(interpret-expression (first target-class)))
    `(new SuperExpr)
    ))

(defn interpret-expression-unary-operation [operator]
  (fn [operand]
    `(new UnaryExpr
          ~(interpret-expression operand)
          ~(japaparser-operator-type-unary operator))))

; there's a slight ambiguity problem with + and -
; they can be unary or binary
; this function must decide which based on how many expressions it is given
(defn interpret-expression-ambiguous-binary-or-unary-operation [operator]
  (fn [operand-l & operand-r]
    (if (first operand-r)
      `(new BinaryExpr
            ~(interpret-expression operand-l)
            ~(interpret-expression (first operand-r))
            ~(japaparser-operator-constant operator))
      `(new UnaryExpr
            ~(interpret-expression operand-l)
            ~(japaparser-operator-constant operator)))))

(defn interpret-expression-binary-operation [operator]
  (fn [operand-l operand-r]
    `(new BinaryExpr
          ~(interpret-expression         operand-l)
          ~(interpret-expression         operand-r)
          ~(japaparser-operator-constant operator))))

(defn interpret-expression-assignment-operation [operator]
  (fn [target value]
    `(new AssignExpr
          ~(interpret-expression         target)
          ~(interpret-expression         value)
          ~(japaparser-operator-constant operator))))

(defn interpret-expression-method-call [target function-name & arguments]
  `(new MethodCallExpr
    ~(interpret-expression target)
    ~(.toString function-name)
    [ ~@(map interpret-expression arguments) ]))

(defn interpret-declarator [form]
  (let [ name        (.toString (nth form 0))
         initializer (first (nthrest form 1))
       ]
    `(new VariableDeclarator
       (new VariableDeclaratorId ~name)
       ~(if initializer (interpret-expression initializer))
        )))

(defn interpret-expression-variable-declaration [modifiers java-type & declarators]
  `(new VariableDeclarationExpr
        ~(interpret-modifiers modifiers)
        ~(interpret-type java-type)
        [ ~@(map interpret-declarator declarators) ] ))

(defn eval-and-interpret [list] (interpret-expression (eval list)))

(comment for debugging
(defn eval-and-interpret [list]
  (interpret-expression "DIDNT MATCH SYNTAX"))
  )

; this is kinda the central point of definition of the syntax of this library
; it associates first-elements of clojure forms
; with functions which interpret those forms as various Java-AST-constructing macros
(def interpreters
  { '(quote .    ) interpret-expression-method-call
    '(quote ==   ) ( interpret-expression-binary-operation '==  )
    '(quote !=   ) ( interpret-expression-binary-operation '!=  )
    '(quote <=   ) ( interpret-expression-binary-operation '<=  )
    '(quote >=   ) ( interpret-expression-binary-operation '>=  )
    '(quote <    ) ( interpret-expression-binary-operation '<   )
    '(quote >    ) ( interpret-expression-binary-operation '>   )
    '(quote <<   ) ( interpret-expression-binary-operation '<<  )
    '(quote >>   ) ( interpret-expression-binary-operation '>>  )
    '(quote >>>  ) ( interpret-expression-binary-operation '>>> )
    '(quote +    ) ( interpret-expression-ambiguous-binary-or-unary-operation '+)
    '(quote -    ) ( interpret-expression-ambiguous-binary-or-unary-operation '-)
    '(quote *    ) ( interpret-expression-binary-operation '*  )
    '(quote /    ) ( interpret-expression-binary-operation '/  )
    '(quote %    ) ( interpret-expression-binary-operation '%  )
    '(quote xor  ) ( interpret-expression-binary-operation 'xor)
    '(quote ||   ) ( interpret-expression-binary-operation '|| )
    '(quote &&   ) ( interpret-expression-binary-operation '&& )
    '(quote |    ) ( interpret-expression-binary-operation '|  )
    '(quote &    ) ( interpret-expression-binary-operation '&  )
    ; assignment expressions
    '(quote =        ) ( interpret-expression-assignment-operation '=       )
    '(quote +=       ) ( interpret-expression-assignment-operation '+=      )
    '(quote -=       ) ( interpret-expression-assignment-operation '-=      )
    '(quote *=       ) ( interpret-expression-assignment-operation '*=      )
    '(quote slash=   ) ( interpret-expression-assignment-operation 'slash=  )
    '(quote &=       ) ( interpret-expression-assignment-operation '&=      )
    '(quote |=       ) ( interpret-expression-assignment-operation '|=      )
    '(quote xor=     ) ( interpret-expression-assignment-operation 'xor=    )
    '(quote %=       ) ( interpret-expression-assignment-operation '%=      )
    '(quote <<=      ) ( interpret-expression-assignment-operation '<<=     )
    '(quote >>=      ) ( interpret-expression-assignment-operation '>>=     )
    '(quote >>>=     ) ( interpret-expression-assignment-operation '>>>=    )
    ; unary operation expressions, except for those that are ambiguous (see above, they are + and -)
    '(quote ++          ) ( interpret-expression-unary-operation '++          )
    '(quote --          ) ( interpret-expression-unary-operation '--          )
    '(quote !           ) ( interpret-expression-unary-operation '!           )
    '(quote bit-inverse ) ( interpret-expression-unary-operation 'bit-inverse )
    '(quote +++         ) ( interpret-expression-unary-operation '+++         )
    '(quote ---         ) ( interpret-expression-unary-operation '---         )
    ; miscellaneous expression
    '(quote super) interpret-expression-super
    '(quote this)  interpret-expression-this
    '(quote new)   interpret-expression-new
    ; local variable declaration
    '(quote local ) interpret-expression-variable-declaration
    })

; eval-and-interpret is the default
; the idea behind that is that it leaves open the possibility of clojure runtime code
; calculating constants that end up as java literals
; or other fun java-compile-time logic
; the thing returned by the arbitrary clojure code you stuff in there
; could be a clojure literal which becomes a java literal expression object expression
; ... but it could also be any other clojure form that is a valid percolator syntax
; ... which is badass extensibility
(defmethod interpret-expression clojure.lang.IPersistentList [list]
  (apply
    (interpreters (first list) eval-and-interpret)
    (drop 1 list)))

(comment 
(interpret-expression 'Balls/cow)
(interpret-expression 23)
(interpret-expression
'('. cow moo "holy fuckin shit")
  )
(interpret-expression
  '( '. System/out println "yes" ))
(interpret-expression '(+ 2 3))
(interpret-expression '( '<= 1 2 ))
  )


