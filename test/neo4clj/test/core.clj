(ns neo4clj.test.core
  (:use [neo4clj.core] :reload-all)
  (:use [clojure.test]))

(use '[clojure.contrib.io :only (delete-file-recursively)])
  
(defn database-fixture [f]
  (try
    (start "var/test")
    (f)
    (finally
      (stop)
      (delete-file-recursively "var/test"))))
  
(use-fixtures :once database-fixture)
    
(deftest Homogeneous-Nodes

  (register-indices :message)
  
  (let [first-node (node! {:message "FirstNode"})
        second-node (node! {:message "SecondNode"})
        third-node (node! {:message "ThirdNode"})
        relation (relate! first-node :knows second-node
                   {:message "ARelation"})
        relation-two (relate! second-node :knows third-node
                       {:message "AnotherRelation"})]
        
    (testing "Properties, dereferencing"
      (are [x y] (= x y)
        {:message "FirstNode"} @first-node
        {:message "ARelation"} @relation
        first-node (start-node relation)))
                                         
    (testing "Traversal"
      (are [x y] (= x y)
        [second-node] (related first-node :knows)
        [] (related first-node :some-label)
        [] (related first-node :knows incoming)
        [second-node] (related first-node :knows outgoing)
        [first-node] (related second-node :knows incoming)
        [first-node third-node] (related second-node :knows)
        [second-node] (related third-node :knows)))
                       
    (testing "Searching"
      (let [search (find-nodes :message "FirstNode")]
        (are [x y] (= x y)
          [first-node] search
          [] (find-nodes :message "ARelation")
          [] (find-nodes :not-an-index "FirstNode"))))
          
    (testing "Altered Properties, Indices"
      (alter! first-node #(assoc % :message "NewFirstNode"))
      (are [x y] (= x y)
        "NewFirstNode" (:message @first-node)
        [] (find-nodes :message "FirstNode")
        [first-node] (find-nodes :message "NewFirstNode")))
          
    (testing "Invalid Transactions"
      (are [x] (thrown? Exception x)
        (delete! first-node)
        (do-tx (failure))))
      
    (testing "Deletion"
      (delete! relation relation-two first-node)
      (is (thrown? Exception @first-node))
      (is (thrown? Exception @relation))
      (is @second-node)
      (delete! second-node third-node)
      (is (thrown? Exception @second-node)))))
      
(deftest Heterogeneous-Nodes

  (register-classes
    :Person [:name :age]
    :Animal [:name :species])
    
  (let [chris-node (node! :Person {:name "Chris" :age 21})
        jim-node (node! :Person {:name "Jim" :age 21})
        domino-node (node! :Animal {:name "Domino" :species "Cat"})]
    
    (testing "Indices"
      (are [x y] (= x y)
        [chris-node] (find-nodes :Person :name "Chris")
        [chris-node jim-node] (find-nodes :Person :age 21)
        [] (find-nodes :Person :name "Domino")
        [domino-node] (find-nodes :Animal :name "Domino")))
    
    (testing "Updating Indicies"
      (alter! chris-node #(assoc % :age 22))
      (are [x y] (= x y)
        1 (count (find-nodes :Person :age 21))
        1 (count (find-nodes :Person :age 22))))
      
    
    (testing "Deleting Indices"
      (are [x y] (= x y)
        nil (delete! chris-node)
        [] (find-nodes :Person :name "Chris")
        [jim-node] (find-nodes :Person :age 21)
        nil (delete! jim-node)
        [] (find-nodes :Person :age 21)
        nil (delete! domino-node)
        [] (find-nodes :Animal :name "Domino")))))
      
(deftest Traversals

  (let [nodes (vec (for [i (range 10)] (node! {:t i})))
        connect (fn [start-id type to-ids]
          (doall (for [target-id to-ids]
            (relate! (nodes start-id) type (nodes target-id)))))]
    
    (let [relations
      (do-tx
        (flatten (list
          (connect 0 :to [1])
          (connect 1 :alt [0])
          (connect 1 :to [2])
          (connect 2 :to [3])
          (connect 3 :to [4])
          (connect 4 :to [5])
          (connect 5 :to [6])
          (connect 6 :to [7])
          (connect 7 :to [8])
          (connect 8 :to [9]))))]
        
      (are [x y z] (= x (count (get-nodes-from (nodes y) z)))
        10 0 (new-traversal)
        
        9  0 (->> (new-traversal)
                  (all-but-start))
        9  0 (->> (new-traversal)
                  (where #(not= (.endNode %) (.startNode %))))
        
        6  0 (->> (new-traversal)
                  (max-depth 5))
        6  0 (->> (new-traversal)
                  (prune #(= (.length %) 5)))
                  
        1  0 single-level-traverse
        2  1 single-level-traverse
                  
        5  5 (->> (new-traversal)
                  (along :to incoming)
                  (all-but-start))
        4  5 (->> (new-traversal)
                  (along :to outgoing)
                  (all-but-start))
        9  5 (->> (new-traversal)
                  (along :to both)
                  (all-but-start))
        0  5 (->> (new-traversal)
                  (along :bad-label outgoing)
                  (all-but-start))
                  
        1  1 (->> (new-traversal)
                  (all-but-start)
                  (along :alt outgoing))
        9  1 (->> (new-traversal)
                  (all-but-start)
                  (along :alt outgoing)
                  (along :to outgoing)))
        
      (apply delete! relations)
      (apply delete! nodes))))
      
(deftest Concurrency
  (let [node-one (node! {:count 0})
        inc-node (fn [node] (alter! node #(update-in % [:count] inc)))]
    
    (testing "Parallel Writes"
      (is (= 0 (@node-one :count)))
      (inc-node node-one)
      (is (= 1 (@node-one :count)))
      (doseq [inc-op (for [i (range 10)]
                (future (inc-node node-one)))]
        @inc-op)
      (is (> 11 (@node-one :count))))
    
    (delete! node-one)
    
    (shutdown-agents)))
      
(deftest Named-Relations
  (register-relations
    [:friends]
    [:requested-friends :friend-requests]
    [:owns :owned-by])
    
  (let [chris-node (node! {:name "Chris"})
        jim-node   (node! {:name "Jim"})
        relation   (relate! chris-node :requested-friends jim-node)]
        
    (testing "Named reversed relations"
      (are [x y] (= x y)
        (list jim-node) (chris-node :requested-friends)
        '() (jim-node :requested-friends)
        (list chris-node) (jim-node :friend-requests)
        '() (chris-node :friend-requests)))
        
    (testing "Homogeneous relations"
      (relate! chris-node :friends jim-node)
      (are [x y] (= x y)
        (list jim-node) (chris-node :friends)
        (list chris-node) (jim-node :friends)))))
        
        
(deftest Array-Properties
  (testing "Array type detection"
    (are [x y] (= x y)
      java.lang.Long (array-storage-type [1 2 3])
      java.lang.Double (array-storage-type [1.2 3])
      java.lang.Boolean (array-storage-type [false true false])
      java.lang.String (array-storage-type ["one" "two"]))
      
    (are [x] (thrown? Exception x)
      (array-storage-type [true 1 2])
      (array-storage-type ["one" 6])))
  
  (testing "Value type preservance"  
    (let [node-one (node! {:doubles [1 2 3.2]
                           :strings '("one" "two" "three")
                           :bools [false true true]
                           :longs [56 1 34]})]
      (are [x y] (= x y)
        [1 2 3.2] (@node-one :doubles)
        ["one" "two" "three"] (@node-one :strings)
        [false true true] (@node-one :bools)
        [56 1 34] (@node-one :longs)
      
        java.lang.Double (type (first (@node-one :doubles)))
        java.lang.Long (type (first (@node-one :longs)))
        java.lang.Boolean (type (first (@node-one :bools))))
    
    (delete! node-one)))
        
  (testing "Array property indexing"
    (register-indices :strings :numbers)
    (let [node-one (node! {:strings ["Elephant Herd" "Giraffe" "Zebra"]
                           :numbers [45 45 9 1]})
          node-two (node! {:numbers [5 9]})]
      (are [x y] (= x y)
        [node-one] (find-nodes :strings "Giraffe")
        [node-one] (find-nodes :strings "Zebra")
        [node-one] (find-nodes :strings "Elephant")
        [node-one] (find-nodes :numbers 45)
        [node-one] (find-nodes :numbers 1)
        [node-two] (find-nodes :numbers 5)
        [node-one node-two] (find-nodes :numbers 9))
        
      (alter! node-one #(update-in % [:numbers] pop))
      (alter! node-two #(update-in % [:numbers] conj 1))
        
      (are [x y] (= x y)
        [node-two] (find-nodes :numbers 1))
        
      (delete! node-one node-two))))