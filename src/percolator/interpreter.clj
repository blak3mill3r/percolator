(in-ns 'percolator.core)

(def interpreters {})
(def scope-inheritance {})

(defn add-interpreters-to-scope [scope new-interpreters]
  (let [ existing (interpreters scope)
         merged   (merge existing new-interpreters) ]
    ( def interpreters (assoc interpreters scope merged))))

(defn inherit-scope [scope parent wrapper]
  (let [ existing  (scope-inheritance scope)
         modified  (conj existing [parent wrapper])
        ]
    (def scope-inheritance
      (assoc scope-inheritance scope modified))))

(defn reset-scope [scope]
  (def interpreters
    (assoc interpreters scope {}))
  (def scope-inheritance
    (assoc scope-inheritance scope [])))

; NOTE the ordering matters if interpreter keys shadow each other
; the LAST one you add with inherit-scope will take precedence in the event of a collision
; undecided if that's Good Thing or not, maybe it should be other way around
; in which case this is the place to start
(defn inherited-interpreters [scope]
  (let [ inherited-scopes (scope-inheritance scope)
         interpreters-for-scopes 
         (map #( interpreters ( first %1 ) ) inherited-scopes)
        ]
    (reduce merge {} interpreters-for-scopes) ))

(defn inherited-interpreter-wrapper [scope parent-scope]
  (or
    ((reduce merge {} (scope-inheritance scope))
     parent-scope)
    identity ))

;(defn kkkkk [scope]
;  (let [ inherited-scopes      (scope-inheritance scope)
;         inherited-scope-names (map first inherited-scopes)
;         interpreters-for-scopes 
;           (map #(
;                  interpreters ( first %1 )
;                  ) inherited-scopes)
;         
;        ]
;    interpreters-for-scopes))
;    ;(reduce merge {} interpreters-for-scopes) ))
;(map #( interpreters ( first %1 ) ) (scope-inheritance :test))
;interpreters
;(map println interpreters)

(defn gimmegimme []
  (reduce merge {} 
    (reverse
      (map (fn [a] (reduce
                     (fn [map-to-scope-name [k v]] (assoc map-to-scope-name k (first a)))
                     {}
                     (last a)))
      interpreters))))

;(gimmegimme)
;
;        (reduce (fn [altered-map [k v]] (assoc altered-map k (f v))) {} m)
;interpreters
;
;(map #(%2) interpreters)
;
;(def ppp (kkkkk :test))
;ppp
;
;scope-inheritance

;(let [set #(if (set? %) % #{%})]
;  #(clojure.set/union (set %) (set %2)))
;
;(reduce merge )
;(merge {} {})
;
;(let [map #(if (map? %) % {})]
;  #(merge (map %1) (map %2)))
;
;( ( (interpreters :testparent) 'foo) )
;( ( (interpreters :testotherparent) 'foo) )
;(add-interpreters-to-scope :testparent
;                           {
;                            'foo (fn [] 3)
;                            })
;(add-interpreters-to-scope :testotherparent
;                           {
;                            'woo (fn [] 4)
;                            })

;(interpret-in-scope :test
;                    '('foo ))

;scope-inheritance
;(((inherited-interpreters :test) 'foo) )
;( (inherited-interpreter-wrapper :test :testparent) 1)
;( (inherited-interpreter-wrapper :test :testotherparent) 1)
;( ( (inherited-interpreters :test) 'foo))
;(first (inherited-interpreters :test))
;( ( (first (inherited-interpreters :test)) 'foo))
;( ( (last (inherited-interpreters :test)) 'foo))
;
;(add-interpreters-to-scope :test
;                           { 'war (fn [] 1) }
;                           )
;(inherit-scope :test :testparent (fn [x] x))
;(inherit-scope :test :testotherparent (fn [x] (+ x 100)))
;;
;(interpreters :test)
;scope-inheritance
;
;(reset-scope :test)


(defn extract-interpreter-key-from-form [form]
  (if
    (seq? form)
    (if
      ( = 'quote (first form) ) 
        (last form))))

(defn interpreter-for-scope-and-form [scope form]
  (let [ root-interpreters (or (scope interpreters) {})
         interpreter-key   (if (seq? form) (extract-interpreter-key-from-form (first form)) (class form))
        ]
    (or
      (root-interpreters interpreter-key)
      (let [ inherited
               (( inherited-interpreters scope) interpreter-key)
             scope-inherited-from
               ( (gimmegimme) interpreter-key )
             wrapper
               ( inherited-interpreter-wrapper scope scope-inherited-from )
            ]
        (do
          (println "scope")
          (println scope)
          (println "form")
          (println form)
          (println "inherited")
          (println inherited)
          (println "scope inherited from")
          (println scope-inherited-from)
          (println "wrapper")
          (println wrapper)
          (println "wrapper called with 1")
          (println ( wrapper 1 ))
          (if inherited (fn [form] (wrapper (inherited form)))) )))))

(defn interpret-in-scope [scope form]
  (let [ interpreter            (interpreter-for-scope-and-form scope form)
         interpreter-arguments  (if (seq? form) (drop 1 form) [form]) ]
    (if interpreter
      ( let [ interpreter-result ( apply interpreter interpreter-arguments) ]
        ( if (interpreter-for-scope-and-form scope interpreter-result)
          (recur scope interpreter-result) interpreter-result ))
      ( let [ eval-result (eval form) ]
        ( if (interpreter-for-scope-and-form scope eval-result)
          (recur scope eval-result) eval-result )))))

;(interpret-in-scope :expression
;                    '('break)
;                    )


;(interpret-in-scope :expression
;                    5.0
;                    "FOobar"
;                    5
;                    )

