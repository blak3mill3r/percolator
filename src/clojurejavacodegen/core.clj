(ns clojurejavacodegen.core
  (:use [clojure.contrib.string :as string]))

(import '(japa.parser.ast CompilationUnit))
(import '(japa.parser.ast PackageDeclaration))
(import '(japa.parser ASTHelper))
(import '(japa.parser.ast.body ClassOrInterfaceDeclaration))
(import '(japa.parser.ast.body ModifierSet))
(import '(japa.parser.ast.body MethodDeclaration))
(import '(japa.parser.ast.body Parameter))
(import '(japa.parser.ast.stmt BlockStmt))
(import '(japa.parser.ast.stmt ExpressionStmt))
(import '(japa.parser.ast.stmt ReturnStmt))
(import '(japa.parser.ast.expr NameExpr))
(import '(japa.parser.ast.expr FieldAccessExpr))
(import '(japa.parser.ast.expr MethodCallExpr))
(import '(japa.parser.ast.expr StringLiteralExpr))
(import '(japa.parser.ast.expr LongLiteralExpr))

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

(defmethod interpret-expression clojure.lang.IPersistentList [list]
  (if (= '. (first list))
    (interpret-statement list) ; wrong ...
    (eval list)
    ))

(defmethod interpret-expression clojure.lang.Symbol [symbol]
  (if (re-find #"^\w*\/\w*$" (.toString symbol))
    (let [ name-and-field (string/split #"/" (.toString symbol)) ]
      `(new FieldAccessExpr (new NameExpr ~(first name-and-field)) ~(last name-and-field))
      )
    `(new NameExpr ~(.toString symbol))
    ))

(interpret-expression 'Balls/cow)
(interpret-expression 'cow)
(interpret-expression 23)
(interpret-expression
'(. System/out println "holy fuckin shit" 3)
  )

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
  (let [ target        (nth expr 1)
         function-name (nth expr 2)
         arguments     (map interpret-expression (nthrest expr 3))
       ]
    `(new ExpressionStmt (doto
       (new MethodCallExpr
         ~(interpret-expression target)
         ~(.toString function-name)
         )
       (.setArgs (doto (new java.util.ArrayList)
;                   ~@(map #( .add (literal-string %) ) arguments)
                   ~@(map #( cons '.add [%1] ) arguments)
                   ))
       ))
    ))
; must be a cleaner way to do the above
; the map within the splicing unquote thingy
; don't get the backquote syntax here in the anonymous function
; maybe make it a named function but what would be the name (add-each?)

(defmacro newkkk [& stmt-list]
  `(spit-it-out
       (doto (new BlockStmt)
         (.setStmts (doto (new java.util.ArrayList)
         ~@( map #(cons '.add [%1]) (map interpret-statement stmt-list) )))
     )
     )
  )

(println (newkkk

             (. oof amethod 3 5 "woot")
             (. oof amethod 3 5 "dong")
           (:return (. foo amethod ) )
           
           )
         )
             (interpret-statement '(. oof amethod 3 5 "woot"))

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


