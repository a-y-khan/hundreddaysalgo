(def project 'hundreddaysalgo)

(set-env!
 :project project
 :resource-paths #{"src"}
 :dependencies '[[org.clojure/clojure "1.10.1"]
                 [org.clojure/test.check "1.0.0"]])

(task-options!
 pom {:project 'hundreddaysalgo
      :version "0.0.1"
      :description "One hundred days of algorithms"}
 jar {:manifest {"hundreddaysalgo" "learn"}})

(deftask build
  "Build my project"
  []
  (comp (pom) (jar) (install)))
