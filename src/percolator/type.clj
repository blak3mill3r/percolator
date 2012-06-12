(in-ns 'percolator.core)
(def primitive-type
  { "boolean"   'japa.parser.ASTHelper/BOOLEAN_TYPE
    "char"      'japa.parser.ASTHelper/CHAR_TYPE
    "byte"      'japa.parser.ASTHelper/BYTE_TYPE
    "short"     'japa.parser.ASTHelper/SHORT_TYPE
    "int"       'japa.parser.ASTHelper/INT_TYPE
    "long"      'japa.parser.ASTHelper/LONG_TYPE
    "float"     'japa.parser.ASTHelper/FLOAT_TYPE
    "double"    'japa.parser.ASTHelper/DOUBLE_TYPE
    "void"      'japa.parser.ASTHelper/VOID_TYPE
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

