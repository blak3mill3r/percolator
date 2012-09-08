(in-ns 'percolator.core)

(reset-scope :body-decl)

(defn interpret-body-decl [form] (interpret-in-scope :body-decl form))

(defn interpret-package-declaration [form]
  `(new PackageDeclaration ~(interpret-expression form)) )

(defn interpret-import-decl [form]
  `(new ImportDeclaration
      ~(interpret-expression form) false false))

(defn interpret-parameter [param-type param-name & array-count-or-varargs]
  (let [ param-type             (reference-type (interpret-type param-type))
         param-name             (.toString param-name)
         array-count-or-varargs (first array-count-or-varargs)
         param-construction     `( japa.parser.ASTHelper/createParameter ~param-type ~param-name )
       ]
    (case array-count-or-varargs
      ...  `(doto ~param-construction (.setVarArgs true))
      nil  param-construction)))

(defn annotation-member-value-pairs [ name-value-pairs ]
  (map (fn [p]
         (let [ name       (.toString (key p))
                value-expr (interpret-expression ( val p ) ) ]
           `(new MemberValuePair "chong" ~value-expr )))
       name-value-pairs))

(defn interpret-annotation [& args]
  (let [ name-expr `(new NameExpr ~(.toString (first args)))
         goods     (first (nthrest args 1)) ]
    (if (map? goods)
       `(new NormalAnnotationExpr ~name-expr [~@(annotation-member-value-pairs goods)] )
       (if goods
         `(new SingleMemberAnnotationExpr ~name-expr ~(interpret-expression goods))
         `(new MarkerAnnotationExpr ~name-expr )))))

(defn interpret-body-decl-method [modifiers-and-annotations return-type method-name param-list & body]
  (let [body-block (when body (interpret-block body))
        { :keys [modifiers annotations throws]} (extract-modifiers-and-annotations modifiers-and-annotations) ]
    `(doto
       (new MethodDeclaration
         nil ;(new JavadocComment "FUCK THIS METHOD") 
         ~modifiers
         [ ~@(map #(apply interpret-annotation %1) annotations) ]
         nil ; type parameters
         ~(interpret-type return-type)
         ~(.toString method-name)
         [ ~@(map #(apply interpret-parameter %1) param-list) ]
         0 ; array count
         ~throws
         ~body-block
         )) ))

(defn interpret-body-decl-ctor [modifiers-and-annotations method-name param-list & body]
  (let [body-block (when body (interpret-block body))
        { :keys [modifiers annotations throws]} (extract-modifiers-and-annotations modifiers-and-annotations) ]
    `(doto
       (new ConstructorDeclaration
         nil ;(new JavadocComment "FUCK THIS METHOD") 
         ~modifiers
         [ ~@(map #(apply interpret-annotation %1) annotations) ]
         nil ; type parameters
         ~(.toString method-name)
         [ ~@(map #(apply interpret-parameter %1) param-list) ]
         ~throws
         ~body-block
         )) ))

;FIXME add support for annotations javadoc etc
(defn interpret-body-decl-field [modifiers-and-annotations java-type & declarators]
  (let [ { :keys [modifiers annotations]} (extract-modifiers-and-annotations modifiers-and-annotations ) ]
    `(new FieldDeclaration
      ~modifiers
      ~(interpret-type java-type)
      [ ~@(map #(apply interpret-declarator %1) declarators) ] ) )) 

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

(defn interpret-body-decl-class [modifiers-and-annotations class-name & body-decls]
  (let [ { :keys [class-modifier-options body-decls]} (snip-class-modifier-options-from-body-decls body-decls)
         { :keys [implements-list extends-list]} (interpret-class-modifier-options class-modifier-options)
         { :keys [modifiers annotations]} (extract-modifiers-and-annotations modifiers-and-annotations )  
        ]
    `( new ClassOrInterfaceDeclaration
          nil ; javadoc
          ~modifiers
          [ ~@(map #(apply interpret-annotation %1) (:annotations modifiers-and-annotations)) ]
          false ; isInterface
          ~(.toString class-name)
          nil ; list of TypeParameter
          ~( when-not (empty? extends-list)    `[ ~@extends-list ])
          ~( when-not (empty? implements-list) `[ ~@implements-list ])
          [ ~@( map interpret-body-decl body-decls ) ] )))

;FIXME NOTE exact dupe of above with true isInterface
(defn interpret-body-decl-interface [modifiers-and-annotations class-name & body-decls]
  (let [ { :keys [class-modifier-options body-decls]} (snip-class-modifier-options-from-body-decls body-decls)
         { :keys [implements-list extends-list]} (interpret-class-modifier-options class-modifier-options)
         { :keys [modifiers annotations]} (extract-modifiers-and-annotations modifiers-and-annotations )  ]
    `( new ClassOrInterfaceDeclaration
          nil ; javadoc
          ~modifiers
          [ ~@(map #(apply interpret-annotation %1) annotations) ]
          true ; isInterface
          ~(.toString class-name)
          nil ; list of TypeParameter
          ~( when-not (empty? extends-list)    `[ ~@extends-list ])
          ~( when-not (empty? implements-list) `[ ~@implements-list ])
          [ ~@( map interpret-body-decl body-decls ) ] )))

(add-interpreters-to-scope
  :body-decl
  { 'method interpret-body-decl-method
    'field  interpret-body-decl-field
    'class  interpret-body-decl-class
    'ctor   interpret-body-decl-ctor
   })
