(in-ns 'percolator.core)

(def interpreters {})

(defn reset-scope [scope]
  (def interpreters (assoc interpreters scope {})))

(defn add-interpreters-to-scope [scope new-interpreters]
  (let [ existing (interpreters scope)
         merged   (merge existing new-interpreters) ]
    ( def interpreters (assoc interpreters scope merged))))

(def ^:dynamic *perc-scope-args* {})

(defn tarjan [e v0]
  (let [tarjan-helper 
         (fn self [v s indices lowlinks index sccs]
           (let [main ; the body of the forall loop
                  (fn [[s indices lowlinks index sccs :as state] vp]
                    (cond
                      (nil? (indices vp)) ; Was successor v' visited? 
                        (let [[s indices lowlinks index sccs] 
                                (apply self vp state)
                              lowlinks (assoc lowlinks v (min (lowlinks v) 
                                                           (lowlinks vp)))]
                          [s indices lowlinks index sccs])
                      (some #{vp} s) ; Is v' on the stack? (crude)
                        (let [lowlinks (assoc lowlinks v (min (lowlinks v) 
                                                           (lowlinks vp)))]
                          [s indices lowlinks index sccs])
                      :else state))

                 ; the forall loop is now a reduction on successors
                 [s indices lowlinks index sccs] 
                   (reduce main [(cons v s) 
                                 (assoc indices v index) 
                                 (assoc lowlinks v index) 
                                 (inc index)
                                 sccs] (e v))]

             (if (= (lowlinks v) (indices v))
               ; conjoin the newly found scc to sccs and 
               ; remove its vertices from the stack
               (let [[a b] (split-with #(not= v %) s)
                     sccs (conj sccs (concat a [v]))]
                 [(rest b) indices lowlinks index sccs])
               [s indices lowlinks index sccs])))]

    (last (tarjan-helper v0 nil {} {} 0 []))))

(def scope-dependencies {})

(defn add-interpreters-to-scope [scope new-interpreters]
  (let [ existing (interpreters scope)
         merged   (merge existing new-interpreters) ]
    ( def interpreters (assoc interpreters scope merged))))

(def scope-inheritance-wrappers
  "a map of scope-name keyword keys to maps of scope-name keyword keys to wrapper functions, used internally to store wrappers which are essentially edge-data in the scope dependency graph"
  {})

(defn inherit-scope [one-scope another-scope & wrapper]
  "make /one-scope/ inherit /another-scope/"
  (def scope-dependencies
    (assoc scope-dependencies one-scope
      (merge
        (or (one-scope scope-dependencies) #{})
        another-scope)))
  (when-not (empty? wrapper)
    (def scope-inheritance-wrappers
      (assoc scope-inheritance-wrappers one-scope
             (merge
               (or (one-scope scope-inheritance-wrappers) {})
               {another-scope (first wrapper)} )))))

(defn inherit-scope-recur [scope]
  "make /scope/ inherit itself"
  (inherit-scope scope scope))


(defn- scope-dependency-traversal [scope]
  "return a list defining the search path for interpreters for the given scope"
  (reverse (tarjan scope-dependencies scope)))

(defn interpreters-and-sources-in-scope [scope]
  "return a map of all interpreter keys available for a given scope to values which are 2-element vectors of [interpreter-fn, scope-name], produce an error for name collisions, TODO cache this"
  (for [strongly-connected-scopes (scope-dependency-traversal scope)]
    (letfn [ (complain-on-collision [one another] (throw (Throwable. "Shitty name collision")))
             (keys-to-scope-name [scope-name]
               (reduce #(assoc % %2 scope-name) {}
                 (keys (interpreters scope-name))))
             (name-collision-safe-merge [one another] (merge-with complain-on-collision one another)) ]
          (reduce name-collision-safe-merge
            ( map #(merge-with vector ( interpreters % ) (keys-to-scope-name %) ) strongly-connected-scopes )
            ))))

(defn extract-interpreter-key-from-form [form]
  (if
    (seq? form)
    (if
      ( = 'quote (first form) ) 
        (last form))))

(defn interpret-in-scope [scope form]
  "interpret percolator form /form/ in scope /scope/ returning a Java object representing the AST node(s)"
  (let [interpreters-and-sources
          (interpreters-and-sources-in-scope scope)
        interpreter-arguments   
          (if (seq? form)
            (drop 1 form)
            [form])
        looks-percolatorish     
          (fn [form] (and (seq? form)
            (seq? (first form) )
            (= 'quote (first (first form)))))  ; if no interpreter is found this is used to know whether to error out or eval the form
        interpreter-key         
          (if
            (seq? form)
            (extract-interpreter-key-from-form (first form))
            (class form))
        interpreter-and-source
          (first (take 1 (drop-while not ; find the value from the first key that matches one of the maps in the vector
            (for [scs interpreters-and-sources] (scs interpreter-key)))) )
        interpreter-fn
          (first interpreter-and-source)
        interpreter-source
          (last interpreter-and-source)
        interpreter-wrapper-fn
          (when interpreter-source
            (or
              (when (not (= scope interpreter-source)) ; not from current scope, check for wrapper fn
                (interpreter-source (scope scope-inheritance-wrappers)))
              identity))
        ]
    (if interpreter-fn
      (let [result (interpreter-wrapper-fn ( apply interpreter-fn interpreter-arguments) )
            ;recursive-result (interpret-in-scope scope result)
            ]
        (if (looks-percolatorish result) ; recur until it isn't a valid percolator form, then eval
          (interpret-in-scope scope result)
            result ))
      ; no interpreter found in any inherited scope
      (if ( looks-percolatorish form)
        ( throw (Throwable. (string/join ["Shitty death with sources " (string/join (map #(.toString %) (first (take 1 interpreters-and-sources ) ) ) )  " on form " (.toString form) "\nscope "  scope " no interpreter-fn for key " (.toString interpreter-key ) " which looks like it should be a percolator form"] )))
        (interpreter-wrapper-fn form )
        ;( throw (Throwable. (string/join [ "Doesnt look percolatorish but I dont have a fn for it: " (.toString form ) ])) )
        ) )))
;( throw (Throwable. (string/join ["Shitty death on form " (.toString form) "\n no interpreter-fn for key " (.toString interpreter-key ) " which is a " (.toString (class interpreter-key))] )))
 
