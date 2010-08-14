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
        relation (relate! first-node :knows second-node {:message "ARelation"})
        relation-two (relate! second-node :knows third-node {:message "AnotherRelation"})]
        
    (testing "Properties, dereferencing"
      (are [x y] (= x y)
        {:message "FirstNode"} @first-node
        {:message "ARelation"} @relation
        first-node (start-node relation)))
                                         
    (testing "Traversal"
      (are [x y] (= x y)
        (list second-node)  (related first-node :knows)
        '()                 (related first-node :some-label)
        '()                 (related first-node :knows incoming)
        (list second-node)  (related first-node :knows outgoing)
        (list first-node)   (related second-node :knows incoming)
        (list second-node)  (related third-node :knows)))
                       
    (testing "Searching"
      (let [search (find-nodes :message "FirstNode")]
        (are [x y] (= x y)
          1 (count search)
          (list first-node) search
          '() (find-nodes :message "ARelation")
          '() (find-nodes :not-an-index "FirstNode"))))
          
    (testing "Altered Properties, Indices"
      (alter! first-node #(assoc % :message "NewFirstNode"))
      (are [x y] (= x y)
        "NewFirstNode" (:message @first-node)
        '() (find-nodes :message "FirstNode")
        (list first-node) (find-nodes :message "NewFirstNode")))
          
    (testing "Invalid Transactions"
      (are [x] (thrown? Exception x)
        (delete! first-node)
        (do-tx (failure))))
      
    (testing "Deletion"
      (do-tx
        (delete! relation)
        (delete! relation-two)
        (delete! first-node))
      (is (thrown? Exception @first-node))
      (is (thrown? Exception @relation))
      (is @second-node)
      (delete! second-node)
      (delete! third-node)
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
        1 (count (find-nodes :Person :name "Chris"))
        2 (count (find-nodes :Person :age 21))
        0 (count (find-nodes :Person :name "Domino"))
        1 (count (find-nodes :Animal :name "Domino"))))
    
    (testing "Updating Indicies"
      (alter! chris-node #(assoc % :age 22))
      (are [x y] (= x y)
        1 (count (find-nodes :Person :age 21))
        1 (count (find-nodes :Person :age 22))))
      
    
    (testing "Deleting Indices"
      (delete! chris-node)
      (is (= 0 (count (find-nodes :Person :name "Chris"))))
      (is (= 1 (count (find-nodes :Person :age 21))))
      (delete! jim-node)
      (is (= 0 (count (find-nodes :Person :age 21))))
      (delete! domino-node)
      (is (= 0 (count (find-nodes :Animal :name "Domino")))))))
      
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
                  (prune (fn [path] (= (.length path) 5))))
                  
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
        
      (doall (map delete! relations))
      (doall (map delete! nodes)))))
      
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
        (list chris-node) (jim-node :friend-requests)))
        
    (testing "Homogeneous relations"
      (relate! chris-node :friends jim-node)
      (are [x y] (= x y)
        (list jim-node) (chris-node :friends)
        (list chris-node) (jim-node :friends)))))