# neo4clj

Comprehensive bindings and utilities for using Neo4j from Clojure.

## Still to come

* Named relationships
* Blocking transactions
* STM-esque transactions (harder)
* More comprehensive tests

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
        
## Transactions

Operations with side-effects (node!, relate!, alter!, delete!, ..) are automatically wrapped into small transactions. They can be grouped into larger transactions with (do-tx [& body]). Uncaught exceptions within the transaction will cause the transaction to be aborted. A transaction can also be aborted using (failure).

Deleting a node without deleting its relationships will cause a transaction to fail.

    (with-neo "test"
      (let [test-node (node! {:message "Consistent"})]
        (do-tx
          (alter! test-node #(assoc % :message "Inconsistent"))
          (failure))
        (= "Consistent" (@test-node :message))
        
        (delete! test-node)))
        
Transactions are ACID and last-one-out-wins for concurrent writes to the same node. Being worked on are helpers to create blocking transactions where required (when protection against lost writes are needed).

## Classed Nodes

Included is basic support for classed nodes and indexes, whereby the Class of a node will determine which indices are kept. It also allows for a property with the same name to be in different indices if the classes are different.

    (with-neo "test"
      (register-classes
        :Person [:name :age]
        :Animal [:name :species])
        
      (let [chris  (node! :Person {:name "Chris" :age 21})
            domino (node! :Animal {:name "Domino" :species "cat"})]
            
        (= chris  (first (find-nodes :Person :name "Chris")))
        (= domino (first (find-nodes :Animal :name "Domino")))
        (alter! chris #(assoc % :name "Domino"))
        (= chris  (first (find-nodes :Person :name "Domino")))
        (= 1      (count (find-nodes :Person :name "Domino")))
        
        (delete! chris)
        (delete! domino)))
        
## Traversals

Traversals are helper methods around the Traversals framework which is new in Neo4j 1.1. Traversal descriptions are immutable and so defaults can be defined and then specialised for certain traversals.

Syntax for custom where and prune predicates currently depends on knowledge of the Neo4j Node, Relationship and Path Interfaces.

    (->> (new-traversal)         ; Generate a new traversal
         (along :type direction) ; Restrict traversal to type & direction of relationships.
                                 ; Multiple along statements commute.
         (where predicate)       ; Sets the filter for which paths to return.
         (prune predicate)       ; Sets when to stop traversing a path.
         (depth-first)           ; Partner is breadth-first.
         (get-nodes-from node))  ; Executes the traversal starting at node, and returns
                                 ; a sequence of node results.
                                 
    (def single-level-traverse   ; Included as a useful base traversal definition.
      (->> (new-traversal)
           (depth-first)
           (max-depth 1)         ; Helper for a prune on path length == 1
           (all-but-start)))     ; Helper for a where on startNode != endNode
           
    ; Included function for finding neighbours of a node by single type and direction.
    (defn related-via-label [node type direction]
      (->> single-level-traverse
           (along type direction)
           (get-nodes-from node)))
        
## Named Relations

Named relations allow reverse relations to be named, and shortcut syntax to be used. When creating relations, the outgoing relation name should be used (in this example, :knows). Modifying relations like sequences is planned for the future, eg. (conj! (node :knows) new-node)

    (register-relations
      [:friends]
      [:knows :known-by])
      
    (related node :knows outgoing)
    (node :knows)
    
    (related node :knows incoming)
    (node :known-by)
    
    (related node :friends)
    (related node :friends both)
    (node :friends)

## License

Copyright (C) 2010 Chris Spencer

Distributed under the MIT License.
