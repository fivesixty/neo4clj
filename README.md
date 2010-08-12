# neo4clj

Comprehensive bindings and utilities for using Neo4j from Clojure.

## Installation

Still under rapid development, so has not been added to clojars just yet.

## Nodes and index

Database can be started/stopped using (start [path]) (stop), or using (with-neo [path & body]) which wraps the contents in a try, do, finally.

    (with-neo "test"
    
      (register-indices :message)     ; Index the message property of nodes.
    
      (let [test-node (node! {:message "Hello"})              ; Create a new node.
            node-map @test-node                               ; Dereference node into a map
            found-node (first (find-nodes :message "Hello"))] ; Find node using index
        
        (alter! test-node #(assoc % :message "Goodbye!"))     ; Update the node
        
        (println (node-map :message))        ; "Hello"
        (println (= test-node found-node))   ; "true"
        (println (@test-node :message))      ; "Goodbye!"
        
        (delete! test-node)))

## Relationships

    (with-neo "test"
      (let [node-one (node! {:message "One"})
            node-two (node! {:message "Two"})
            rel-one  (relate! node-one :knows node-two)
            rel-two  (relate! node-one :has node-two {:since 2010})]
        
        (= node-two (first (related node-one :knows)))  ; true
        (count (related node-two :knows outgoing))      ; 1
        (count (related node-two :knows incoming))      ; 0
        (println "since" (@rel-two :since))             ; "since 2010"
        
        (delete! rel-one) (delete! rel-two)     ; Relationships deleted before nodes.
        (delete! node-one) (delete! node-two)))
        
## License

Copyright (C) 2010 Chris Spencer

Distributed under the MIT License.
