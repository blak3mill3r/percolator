(in-ns 'percolator.core)

; clojure's ISeq seems to realize sequences in "chunks" of 32
; this confused the shit out of me...
; here is a lazy sequence that does not realize more than is requested
; wasted so much time on this ...
; http://stackoverflow.com/questions/10556421/is-for-not-actually-lazy-in-clojure
(defn truly-lazy-seq [s]
  (lazy-seq
    (when-let [[x] (seq s)]
       (cons x (truly-lazy-seq (rest s))))))

(def ^:dynamic percolator.core/*scope-deps*
  (DefaultDirectedGraph.
    (ClassBasedEdgeFactory. clojure.lang.PersistentArrayMap)))

; make /one-scope/ inherit /another-scope/
(defn inherit-scope [one-scope another-scope & wrapper]
  (let [wrapper-fn     (first wrapper)
        new-edge-data  (assoc (if wrapper-fn {:wrapper-fn wrapper-fn} {} ) :inheritance [one-scope another-scope]) ]
    (def ^:dynamic percolator.core/*scope-deps* (do
      (. *scope-deps* addVertex one-scope)
      (. *scope-deps* addVertex another-scope)
      (let [existing-edge-data (. *scope-deps* getEdge one-scope another-scope)]
        (if existing-edge-data (do
          (. *scope-deps* removeEdge one-scope another-scope )
          (. *scope-deps* addEdge one-scope another-scope (merge existing-edge-data new-edge-data )))
        (. *scope-deps* addEdge one-scope another-scope new-edge-data )))
      *scope-deps*
      ))))

; get list of edge maps in order of closeness
; this should be the proper order of precedence for name collision resolution
(defn scope-hierarchy-in-traversal-order [current-scope]
  (let [vertices     (iterator-seq (BreadthFirstIterator. *scope-deps* current-scope) )
        vertex-pairs (partition 2 1 vertices)
        edges        (map #(. *scope-deps* getEdge (first %) (last %)) vertex-pairs)
        ]
    edges))

(inherit-scope :statement :expression (fn [expr] `(new ExpressionStmt ~expr)))
(inherit-scope :gwt-statement :statement)
(inherit-scope :frog :dingus)

(def interpreters {})

(defn directly-inherited-scopes [scope]
  (if
    (. *scope-deps* containsVertex scope)
    (. *scope-deps* outgoingEdgesOf scope)
    []))

(defn extract-interpreter-key-from-form [form]
  (if
    (seq? form)
    (if
      ( = 'quote (first form) ) 
        (last form))))

(defn reset-scope [scope]
  (def interpreters (assoc interpreters scope {})))

(defn add-interpreters-to-scope [scope new-interpreters]
  (let [ existing (interpreters scope)
         merged   (merge existing new-interpreters) ]
    ( def interpreters (assoc interpreters scope merged))))

(def ^:dynamic *perc-scope-args* {})

(defn add-interpreters-to-scope [scope new-interpreters]
  (let [ existing (interpreters scope)
         merged   (merge existing new-interpreters) ]
    ( def interpreters (assoc interpreters scope merged))))


(defn debug-indent [depth]
  (apply str (repeat depth "  ")))

(defn looks-percolatorish [form] (and (seq? form)
            (seq? (first form) )
            (= 'quote (first (first form)))))  ; if no interpreter is found this is used to know whether to error out or eval the form

; should return nil if no interpreter is found in scope or any inherited scope
(defn attempt-interpret-in-scope [scope original-scope form depth]
  (let [interpreter-key
          (if
            (seq? form)
            (extract-interpreter-key-from-form (first form))
            (class form))
        interpreter-arguments   
          (if (seq? form)
            (drop 1 form)
            [form])
        inherited-scopes (truly-lazy-seq (directly-inherited-scopes scope) )
        extract-wrapper-fn (fn [x] ( :wrapper-fn x ))
        extract-inherited-scope (fn [x] (last (:inheritance x)))
        interpreters-in-current-scope (scope interpreters)
        interpret-in-inherited-scope
          (fn [iscope]
            (let [raw-result (attempt-interpret-in-scope (extract-inherited-scope iscope) original-scope form (+ depth 1))
                  wrapper-fn (or (extract-wrapper-fn iscope) identity )]
              (when raw-result
                  (wrapper-fn raw-result)
                  )))
        interpreter-fn-in-current-scope
          (interpreters-in-current-scope interpreter-key)
        interpret-in-current-scope
          (fn []
            (when interpreter-fn-in-current-scope
              (let [result (apply interpreter-fn-in-current-scope interpreter-arguments )]
                (if (looks-percolatorish result)
                  (attempt-interpret-in-scope original-scope original-scope result (+ 1 depth)) ; recur if the result of interpretation is another percolator form
                  result))))
       ]
    (or
      (interpret-in-current-scope)
      (first (take 1 (filter identity (map interpret-in-inherited-scope inherited-scopes))))
      )))

(defn interpret-in-scope [scope form]
  (or
    (attempt-interpret-in-scope scope scope form 0) ; it matched some interpreter
      (if (looks-percolatorish form) ; no interpreter found
        (throw ( Throwable. (apply str "No interpreter found in scope " scope " for the form " form))) ; and it looks like percolator syntax
        form ))) ; otherwise it's just clojure

