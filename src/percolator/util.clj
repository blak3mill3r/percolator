(ns percolator.util
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
    )
  )

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

