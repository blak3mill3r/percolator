(in-ns 'percolator.core)

; symbol aliases
; which resolves to an Operator constant from japaparser
(def japaparser-operator-constant
  { '==      'japa.parser.ast.expr.BinaryExpr$Operator/equals
    '!=      'japa.parser.ast.expr.BinaryExpr$Operator/notEquals
    '<=      'japa.parser.ast.expr.BinaryExpr$Operator/lessEquals
    '>=      'japa.parser.ast.expr.BinaryExpr$Operator/greaterEquals
    '<       'japa.parser.ast.expr.BinaryExpr$Operator/less
    '>       'japa.parser.ast.expr.BinaryExpr$Operator/greater
    '<<      'japa.parser.ast.expr.BinaryExpr$Operator/lShift
    '>>      'japa.parser.ast.expr.BinaryExpr$Operator/rSignedShift
    '>>>     'japa.parser.ast.expr.BinaryExpr$Operator/rUnsignedShift
    '+       'japa.parser.ast.expr.BinaryExpr$Operator/plus
    '-       'japa.parser.ast.expr.BinaryExpr$Operator/minus
    '*       'japa.parser.ast.expr.BinaryExpr$Operator/times
    '/       'japa.parser.ast.expr.BinaryExpr$Operator/divide
    '%       'japa.parser.ast.expr.BinaryExpr$Operator/remainder
    'xor     'japa.parser.ast.expr.BinaryExpr$Operator/xor
    '||      'japa.parser.ast.expr.BinaryExpr$Operator/or
    '&&      'japa.parser.ast.expr.BinaryExpr$Operator/and
    '|       'japa.parser.ast.expr.BinaryExpr$Operator/binOr
    '&       'japa.parser.ast.expr.BinaryExpr$Operator/binAnd
   ; assigment
    '=       'japa.parser.ast.expr.AssignExpr$Operator/assign
    '+=      'japa.parser.ast.expr.AssignExpr$Operator/plus
    '-=      'japa.parser.ast.expr.AssignExpr$Operator/minus
    '*=      'japa.parser.ast.expr.AssignExpr$Operator/star
    'slash=  'japa.parser.ast.expr.AssignExpr$Operator/slash  ; irritating, reader pissed at '\= (  but not at '\  )
    '&=      'japa.parser.ast.expr.AssignExpr$Operator/and
    '|=      'japa.parser.ast.expr.AssignExpr$Operator/or
    'xor=    'japa.parser.ast.expr.AssignExpr$Operator/xor
    '%=      'japa.parser.ast.expr.AssignExpr$Operator/rem
    '<<=     'japa.parser.ast.expr.AssignExpr$Operator/lShift
    '>>=     'japa.parser.ast.expr.AssignExpr$Operator/rSignedShift
    '>>>=    'japa.parser.ast.expr.AssignExpr$Operator/rUnsignedShift
  })

; has to be distinct from the above because the names collide
; e.g. '- can be a unary negation or a subtraction
(def japaparser-operator-type-unary
  { '+            'japa.parser.ast.expr.UnaryExpr$Operator/positive
    '-            'japa.parser.ast.expr.UnaryExpr$Operator/negative
    '++           'japa.parser.ast.expr.UnaryExpr$Operator/preIncrement
    '--           'japa.parser.ast.expr.UnaryExpr$Operator/preDecrement
    '!            'japa.parser.ast.expr.UnaryExpr$Operator/not
    'bit-inverse  'japa.parser.ast.expr.UnaryExpr$Operator/inverse          ; reader pissed about '~
    '+++          'japa.parser.ast.expr.UnaryExpr$Operator/posIncrement     ; order dictates difference in java ... not going to go there
    '---          'japa.parser.ast.expr.UnaryExpr$Operator/posDecrement
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

(defn split-arguments-and-body-decls [forms]
  (let [ arguments-and-body-decls
         (partition-by
           #( boolean ( (first-matches #{ '(quote field) '(quote method) })
                        %1 ))
          forms) ]
    (if (and (= 1 (count arguments-and-body-decls))
             (some (first-matches #{ '(quote field) '(quote method) }) (first arguments-and-body-decls )))
      { :body-decls (first arguments-and-body-decls) }
      { :arguments (first arguments-and-body-decls)
        :body-decls (first (drop 1 arguments-and-body-decls))
        })))

(defn interpret-expression-new [type-name & arguments-and-maybe-anonymous-class-body]
  (let [{:keys [body-decls arguments]} (split-arguments-and-body-decls arguments-and-maybe-anonymous-class-body)]
    `(doto
       (new ObjectCreationExpr
         nil  ; FIXME this argument, scope, can be used for instantiating outer classes from inner classes
          ~(interpret-type type-name)
          [ ~@(map interpret-expression arguments) ]
          )
        (.setAnonymousClassBody ~( when body-decls `[ ~@( map interpret-body-decl body-decls ) ] ))
      )))

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

(defn interpret-declarator [name & initializer]
  `(new VariableDeclarator
     (new VariableDeclaratorId ~(.toString name))
     ~(if (first initializer) (interpret-expression (first initializer)))
      ))

(defn interpret-expression-variable-declaration [modifiers java-type & declarators]
  `(new VariableDeclarationExpr
        ~(interpret-modifiers modifiers)
        ~(interpret-type java-type)
        [ ~@(map #(apply interpret-declarator %1) declarators) ] ))

(defn eval-and-interpret [list] (interpret-expression (eval list)))

(comment for debugging
(defn eval-and-interpret [list]
  (interpret-expression "DIDNT MATCH SYNTAX"))
  )

(defn interpret-expression-class [java-type]
  `(new ClassExpr ~(interpret-type java-type)))

; this is kinda the central point of definition of the syntax of this library
; it associates first-elements of clojure forms
; with functions which interpret those forms as various Java-AST-constructing macros
(def expression-interpreters
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
    ; class expression, i.e. Something.class
    '(quote class-expr ) interpret-expression-class
    })

; eval-and-interpret is the default
; the idea behind that is that it leaves open the possibility of clojure runtime code
; calculating constants that end up as java literals
; or other fun java-compile-time logic
; the thing returned by the arbitrary clojure code you stuff in there
; could be a clojure literal which becomes a java literal expression object expression
; ... but it could also be any other clojure form that is a valid percolator syntax
; ... which is badass extensibility
(defmethod interpret-expression clojure.lang.IPersistentList [form]
  (let [ expression-interpreter (expression-interpreters (first form))
         interpreter-arguments  (drop 1 form) ]
    (if expression-interpreter
      ( apply expression-interpreter interpreter-arguments )
      (interpret-expression (eval form)))))
