(in-ns 'percolator.core)
(declare body-decl-interpreters)

(defn interpret-package-declaration [form]
  `(new PackageDeclaration ~(interpret-expression form)))

(defn interpret-import-decl [form]
  `(new ImportDeclaration
      ~(interpret-expression form) false false))

(apply interpret-import-decl '(
 duck.balls.woot.*
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

(defn interpret-body-decl-ctor [modifiers method-name param-list & body]
  `(doto
     (new ConstructorDeclaration
          nil ; javadoc
          ~(interpret-modifiers modifiers)
          nil ;annotations
          nil ;type parameters
          ~(.toString method-name)
          [ ~@(map #(apply interpret-parameter %1) param-list) ]
          nil ;throws
          ~(interpret-block body))))

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

; class modifier options are forms that live in the class body forms
; along with body-decls
; but they aren't body-decls at all, they affect the enclosing class
; used for implements and extends at the moment
(defn interpret-class-modifier-options [class-modifier-options]
  { :implements-list
      (interpret-class-modifier-option (first-form-that-looks-like #{ '(quote implements) } class-modifier-options))
    :extends-list
      (interpret-class-modifier-option (first-form-that-looks-like #{ '(quote extends) } class-modifier-options))
   })

(defn interpret-body-decl-class [modifiers class-name & body-decls]
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
          ~( when-not (empty? extends-list)    `[ ~@extends-list ])
          ~( when-not (empty? implements-list) `[ ~@implements-list ])
          [ ~@( map interpret-body-decl body-decls ) ] )))

(def body-decl-interpreters
  { '(quote method) interpret-body-decl-method
    '(quote field)  interpret-body-decl-field
    '(quote class)  interpret-body-decl-class
    '(quote ctor)   interpret-body-decl-ctor
    })

; keep in mind that all body declarations share 2 things in common
; they can have javadocs and they can have annotations
; make sure that's generic in syntax and implementation
