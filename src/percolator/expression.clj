(in-ns 'percolator.core)

(reset-scope :expression)

(defn interpret-expression [form] (interpret-in-scope :expression form))

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

; clojure symbols as a shorthand for NameExpr and FieldAccessExpr-of-NameExpr i.e. System or System/out
(defn interpret-expression-symbol [symbol]
  (if (re-find #"^\w*\/\w*$" (.toString symbol))
    (let [ name-and-field (string/split #"/" (.toString symbol)) ]
      `(new FieldAccessExpr (new NameExpr ~(first name-and-field)) ~(last name-and-field)))
    `(new NameExpr ~(.toString symbol))))

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
    `(doto (new ObjectCreationExpr
             nil  ; FIXME this argument, scope, can be used for instantiating outer classes from inner classes
              ~(interpret-type type-name)
              [ ~@(map interpret-expression arguments) ]
              )
            (.setAnonymousClassBody ~( when body-decls `[ ~@( map interpret-body-decl body-decls ) ] )))))

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
            ~(japaparser-operator-type-unary operator)))))

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

(defn interpret-expression-class [java-type]
  `(new ClassExpr ~(interpret-type java-type)))

(add-interpreters-to-scope
  :expression
  { 'new                interpret-expression-new
    'this               (fn [] `(new ThisExpr))
    'super              interpret-expression-super
    '.                  interpret-expression-method-call
    'local              interpret-expression-variable-declaration
    'class-expr         interpret-expression-class
    java.lang.String    (fn [string] `(new StringLiteralExpr  ~string)              )
    java.lang.Long      (fn [long]   `(new LongLiteralExpr    ~(.toString long)    ))
    java.lang.Boolean   (fn [bool]   `(new BooleanLiteralExpr ~bool                ))
    java.lang.Character (fn [char]   `(new CharLiteralExpr    ~(.toString char)    ))
    java.lang.Double    (fn [double] `(new DoubleLiteralExpr  ~(.toString double)  ))   ; to add floats as well will require some extra percolator syntax hint, ditto int types smaller than long
    clojure.lang.Symbol interpret-expression-symbol
    '++                 ( interpret-expression-unary-operation '++          )
    '--                 ( interpret-expression-unary-operation '--          )
    '!                  ( interpret-expression-unary-operation '!           )
    'bit-inverse        ( interpret-expression-unary-operation 'bit-inverse )
    '+++                ( interpret-expression-unary-operation '+++         )
    '---                ( interpret-expression-unary-operation '---         )
    '+                  ( interpret-expression-ambiguous-binary-or-unary-operation '+)
    '-                  ( interpret-expression-ambiguous-binary-or-unary-operation '-)
    '==                 ( interpret-expression-binary-operation '==  ) 
    '!=                 ( interpret-expression-binary-operation '!=  )
    '<=                 ( interpret-expression-binary-operation '<=  )
    '>=                 ( interpret-expression-binary-operation '>=  )
    '<                  ( interpret-expression-binary-operation '<   )
    '>                  ( interpret-expression-binary-operation '>   )
    '<<                 ( interpret-expression-binary-operation '<<  )
    '>>                 ( interpret-expression-binary-operation '>>  )
    '>>>                ( interpret-expression-binary-operation '>>> )
    '*                  ( interpret-expression-binary-operation '*  )
    '/                  ( interpret-expression-binary-operation '/  )
    '%                  ( interpret-expression-binary-operation '%  )
    'xor                ( interpret-expression-binary-operation 'xor)
    '||                 ( interpret-expression-binary-operation '|| )
    '&&                 ( interpret-expression-binary-operation '&& )
    '|                  ( interpret-expression-binary-operation '|  )
    '&                  ( interpret-expression-binary-operation '&  )
    '=                  ( interpret-expression-assignment-operation '=       )
    '+=                 ( interpret-expression-assignment-operation '+=      )
    '-=                 ( interpret-expression-assignment-operation '-=      )
    '*=                 ( interpret-expression-assignment-operation '*=      )
    'slash=             ( interpret-expression-assignment-operation 'slash=  )
    '&=                 ( interpret-expression-assignment-operation '&=      )
    '|=                 ( interpret-expression-assignment-operation '|=      )
    'xor=               ( interpret-expression-assignment-operation 'xor=    )
    '%=                 ( interpret-expression-assignment-operation '%=      )
    '<<=                ( interpret-expression-assignment-operation '<<=     )
    '>>=                ( interpret-expression-assignment-operation '>>=     )
    '>>>=               ( interpret-expression-assignment-operation '>>>=    )
   })
