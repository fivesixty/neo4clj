# neo4clj

Comprehensive bindings and utilities for using Neo4j from Clojure.

## Installation

Still under rapid development, so has not been added to clojars just yet.

## Nodes

Database can be started/stopped using (start [path]) (stop), or using (with-neo [path & body]) which wraps the contents in a try, do, finally.

    (with-neo "test"
    
      (register-indices :message)
      ; Index the message property of nodes.
    
      (let [test-node (node! {:message "Hello"})
            node-map @test-node]
        
        (println (node-map :message))
        ; Hello
        
        (println (count (find-nodes :message "Hello")) "nodes")
        ; Search for nodes with a message containing Hello.
        
        (alter! test-node #(assoc % :message "Goodbye!"))
        
        (println (node-map :message))
        ; Hello
        (println (@test-node :message))
        ; Goodbye!
        
        (delete! test-node)))

## License

Copyright (C) 2010 Chris Spencer

Distributed under the MIT License.
