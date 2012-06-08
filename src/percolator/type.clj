(in-ns 'percolator.core)
(def primitive-type
  { "boolean"   'ASTHelper/BOOLEAN_TYPE
    "char"      'ASTHelper/CHAR_TYPE
    "byte"      'ASTHelper/BYTE_TYPE
    "short"     'ASTHelper/SHORT_TYPE
    "int"       'ASTHelper/INT_TYPE
    "long"      'ASTHelper/LONG_TYPE
    "float"     'ASTHelper/FLOAT_TYPE
    "double"    'ASTHelper/DOUBLE_TYPE
    "void"      'ASTHelper/VOID_TYPE
   })

(defn interpret-type [symbol]
  (let [ name (.toString symbol)
         primitive (primitive-type name)
       ]
    (if primitive
      primitive
      `(new ClassOrInterfaceType
            nil   ; FIXME TODO this argument, scope, will be needed for instantiating outer classes from inner classes
            ~name))
      ))

(defn reference-type [type]
  `(new ReferenceType ~type 0))
; FIXME 0 is the array count , 0 meaning its not an array TODO support arrays

