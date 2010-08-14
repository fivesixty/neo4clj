(defproject neo4clj "0.0.1-SNAPSHOT"
  :description "Clojure 1.2 bindings for Neo4j 1.1"
  :dependencies [[org.clojure/clojure "1.2.0-RC3"]
                 [org.clojure/clojure-contrib "1.2.0-RC3"]
                 [org.neo4j/neo4j-kernel "1.1"]
                 [org.neo4j/neo4j-index "1.1"]]
  ; :dev-dependencies [[swank-clojure "1.2.1"]]
  :repositories {"neo4j-public-repository" "http://m2.neo4j.org"}
  
  :warn-on-reflection true
  :aot [neo4clj.core])