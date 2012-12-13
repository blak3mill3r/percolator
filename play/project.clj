(defproject play "1.0.0-SNAPSHOT"
  :description "for building GWT interfaces with clojure"
  :dependencies [
                 [org.clojure/clojure                   "1.4.0" ]
                 [com.google.code.javaparser/javaparser "1.0.8" ]
                 [percolator/percolator                 "1.0.0-SNAPSHOT" ]
                 ]
  :dev-dependencies [
                     [org.clojars.ibdknox/lein-nailgun "1.1.1" ]
                     [vimclojure/server                "2.3.5" ]
                     [lein-gwt                         "0.1.1" ]
                     [lein-percolator                  "0.0.1-SNAPSHOT" ]
                     [com.google.gwt/gwt-user          "2.4.0" ]
                     [com.google.gwt/gwt-dev           "2.4.0" ]
                     ]
  :gwt-modules ["com.whatsys.Play"]
  :gwt-options {:localWorkers 1
                :war "war"
                :logdir "log"
                :logLevel INFO
                :strict ""
                }
  :gwt-options-devmode {}
  :percolator-options {}
  :percolator-namespaces [com.whatsys.test com.whatsys.tawrongle]
  :source-path "src"
            )
