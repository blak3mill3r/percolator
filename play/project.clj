(defproject play "1.0.0-SNAPSHOT"
  :description "for building GWT interfaces with clojure"
  :dependencies [
                 [org.clojure/clojure                   "1.5.1" ]
                 [com.google.code.javaparser/javaparser "1.0.8" ]
                 [percolator/percolator                 "1.0.0-SNAPSHOT" ]
                 [com.google.gwt/gwt-user               "2.5.1" ]
                 [com.google.gwt/gwt-dev                "2.5.1" ]
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
