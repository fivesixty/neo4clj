(ns neo4clj.core
  (:import (org.neo4j.graphdb Direction
                              Node
                              Relationship
                              RelationshipType
                              Transaction
                              Path
                              PropertyContainer
                              NotFoundException
                              TransactionFailureException)
	   (org.neo4j.kernel EmbeddedGraphDatabase
                       Traversal)
     (org.neo4j.graphdb.traversal TraversalDescription
                                  Traverser
                                  TraversalBranch
                                  PruneEvaluator)
     (org.neo4j.graphdb.event TransactionEventHandler
                              PropertyEntry)
     (org.neo4j.index.lucene LuceneFulltextIndexService)
     (org.neo4j.helpers Predicate)))
     
; Neo Database
        
(def ^EmbeddedGraphDatabase *neo* nil)
(def ^LuceneFulltextIndexService *lucene* nil)
(def *named-relations* {})
(def *classes* {})
(def *indices* #{})

(defn attach-index-handler [])
(defn attach-classed-index-handler [])

(defn start
  "Start a neo4j instance from the given database path, and bind to *neo*"
  [path]
  (do
    (alter-var-root #'*neo* (fn [_] (EmbeddedGraphDatabase. path)))
    (alter-var-root #'*lucene* (fn [_] (LuceneFulltextIndexService. *neo*)))
    (attach-index-handler)
    (attach-classed-index-handler)))
  
(defn stop
  "Stop the running neo4j instance and unbind *neo*"
  []
  
  (do
    (.shutdown *neo*)
    (.shutdown *lucene*)
    (alter-var-root #'*neo* (fn [g] nil))
    (alter-var-root #'*lucene* (fn [g] nil))))
                             
(defmacro with-neo
  "Wrap body inside a neo-start, neo-stop
  Useful for test harness."
  [path & body]
  
  `(do
    (start ~path)
    (try
      (do ~@body)
      (finally
        (stop)))))
     
; Transactions
     
(def ^Transaction *tx* nil)

(defn success []
  (.success *tx*))
(defn failure []
  (.failure *tx*))
(defn finish []
  (.finish *tx*))
  
(defmacro do-tx [& body]
  `(if *tx* (do ~@body)
    (binding [*tx* (.beginTx *neo*)]
    (try
      (let [val# (do ~@body)]
        (success)
        val#)
      (finally
        (finish))))))
     
; Protocols
     
(defprotocol PElement
  (alter! [this f])
  (delete! [this]))
     
(defprotocol PNode
  (relate! [this type to] [_ type to properties])
  (related [this type] [this type direction]))
  
(defprotocol PRelationship
  (start-node [this])
  (end-node [this]))
     
(defprotocol PropertyWrap
  (setProperties! [this properties])
  (getProperties [this]))
     
; Types

(deftype Neo-Node [^Node element])     
(deftype Neo-Relationship [^Relationship element])
(deftype Neo-RelationshipType [n]
  RelationshipType
    (name [_] (name n)))
     
(defn- seq-to-array [sequence]
  (if (number? (first sequence))
    (into-array java.lang.Double (map double sequence))
    (into-array java.lang.String (map str sequence))))
     
(extend-type PropertyContainer
  PropertyWrap
  (setProperties! [this properties]
    (doseq [[key value] properties]
      (if (or (seq? value) (vector? value))
        (.setProperty this (name key) (seq-to-array value))
        (.setProperty this (name key) value)))
    this)
  (getProperties [this]
    (let [ks (.getPropertyKeys this)]
      (into {}
        (map
          (fn [k]
            (let [prop (.getProperty this k)]
              (if (or (string? prop) (number? prop))
                [(keyword k) prop]
                [(keyword k) (vec prop)])))
          ks)))))
     
; Traversals

(def both Direction/BOTH)
(def incoming Direction/INCOMING)
(def outgoing Direction/OUTGOING)

(defn ^TraversalDescription max-depth [n ^TraversalDescription traversal]
  (.prune traversal (Traversal/pruneAfterDepth n)))
(defn ^TraversalDescription depth-first [^TraversalDescription traversal]
  (.depthFirst traversal))
(defn ^TraversalDescription breadth-first [^TraversalDescription traversal]
  (.breadthFirst traversal))
  
(defn predicate [f]
  (reify
    Predicate
      (accept [_ item] (f item))))
      
(defn pruner [f]
  (reify
    PruneEvaluator
      (^boolean pruneAfter [_ ^Path path] (f path))))
      
(defn all-but-start [^TraversalDescription traversal]
  (.filter traversal (Traversal/returnAllButStartNode)))
  
(defn where [f ^TraversalDescription traversal]
  (.filter traversal (predicate f)))
  
(defn prune [f ^TraversalDescription traversal]
  (.prune traversal (pruner f)))
  
(defn get-nodes-from [^Neo-Node node ^TraversalDescription traversal]
  (map #(Neo-Node. %) (.nodes (.traverse traversal (.element node)))))
  
(defn along-mult [traversal & relations]
  (reduce (fn [^TraversalDescription traversal [type direction]]
    (.relationships traversal (Neo-RelationshipType. type) direction))
    traversal
    (partition 2 relations)))
    
(defn along [type direction ^TraversalDescription traversal]
  (.relationships traversal (Neo-RelationshipType. type) direction))

(defn new-traversal []
  (Traversal/description))
    
(def single-level-traverse
  (->> (new-traversal)
       (depth-first)
       (max-depth 1)
       (all-but-start)))
      
(defn related-via-label [node type direction]
  (->> single-level-traverse
       (along type direction)
       (get-nodes-from node)))
     
; Node
     
(deftype Neo-Node [^Node element]
  
  Object
    (equals [_ other]
      (= element (.element ^Neo-Node other)))
  
  clojure.lang.IDeref
    (deref [_] (getProperties element))
  
  PNode
    (relate! [this type to] (relate! this type to {}))
    (relate! [_ type to properties]
      (Neo-Relationship. (do-tx
        (let [relation (.createRelationshipTo element (.element ^Neo-Node to) (Neo-RelationshipType. type))]
          (setProperties! relation properties)))))
    (related [this type]
      (related-via-label this type Direction/BOTH))
    (related [this type direction]
      (related-via-label this type direction))
      
  PElement
    (alter! [_ f]
      (do-tx
        (setProperties! element (f (getProperties element)))))
    (delete! [_]
      (do-tx (.delete element)))
      
  clojure.lang.IFn
    (invoke [this type]
      (if (contains? *named-relations* type)
        (related this ((*named-relations* type) :type) ((*named-relations* type) :direction))
        (related this type both))))
    
; Relationship
    
(deftype Neo-Relationship [^Relationship element]
  
  clojure.lang.IDeref
    (deref [_] (getProperties element))
    
  PRelationship
    (start-node [_] (Neo-Node. (.getStartNode element)))
    (end-node [_] (Neo-Node. (.getEndNode element)))
    
  PElement
    (alter! [_ f]
      (do-tx
        (setProperties! element (f (getProperties element)))))
    (delete! [_]
      (do-tx (.delete element))))
    
; Events/Indexing
  
(defmacro transaction-handler [& body]
  `(.registerTransactionEventHandler *neo*
    (reify TransactionEventHandler
      ~@body)))
      
(defn register-indices [& args]
  (alter-var-root #'*indices* #(into % (map name args))))
  
(defn attach-index-handler []
  (transaction-handler
    (afterCommit [_ data state] nil)
    (beforeCommit [_ data]
      (let [remnodes (into #{} (map (fn [^Node node] (.getId node)) (.deletedNodes data)))]
        (doseq [^PropertyEntry removal (.removedNodeProperties data)]
          (let [entity ^Node (.entity removal)
                key (.key removal)]
            (when (contains? *indices* key)
              (.removeIndex *lucene* entity key)))))
      (doseq [^PropertyEntry assign (.assignedNodeProperties data)]
        (let [entity ^Node (.entity assign)
              key (.key assign)
              previous-value (.previouslyCommitedValue assign)]
          (when (contains? *indices* key)
            (when previous-value
              (.removeIndex *lucene* entity key previous-value))
            (.index *lucene* entity key (.value assign))))))
    (afterRollback [_ data state] nil)))
    
(defn- deleted-node-classes [deleted-nodes removed-properties]
  (let [removed-nodes (into {} (map (fn [^Node node] [node {}]) deleted-nodes))]
    (reduce
      (fn [props ^PropertyEntry removal]
        (update-in props [(.entity removal)] assoc (.key removal) (.previouslyCommitedValue removal)))
      removed-nodes
      (filter #(contains? removed-nodes (.entity ^PropertyEntry %)) removed-properties))))

(defn attach-classed-index-handler []
  (letfn [(entity-class [^Node entity]
            (if (.hasProperty entity "__CLASS") (.getProperty entity "__CLASS")))]
    (transaction-handler
      (afterCommit [_ data state] nil)
      (beforeCommit [_ data]
        (let [removed-nodes (deleted-node-classes (.deletedNodes data) (.removedNodeProperties data))]
          (doseq [^PropertyEntry removal (.removedNodeProperties data)]
            (let [entity ^Node (.entity removal)
                  key (.key removal)
                  class (if (removed-nodes entity) ((removed-nodes entity) "__CLASS") (entity-class entity))]
              (when (and class (contains? (*classes* class) key))
                (.removeIndex *lucene* entity (str class "__" key)))))
          (doseq [^PropertyEntry assign (.assignedNodeProperties data)]
            (let [entity ^Node (.entity assign)
                  key (.key assign)
                  class (entity-class entity)
                  previous-value (.previouslyCommitedValue assign)]
              (when (and class (contains? (*classes* class) key))
                (when previous-value
                  (.removeIndex *lucene* entity (str class "__" key) previous-value))
                (.index *lucene* entity (str class "__" key) (.value assign)))))))
      (afterRollback [_ data state] nil))))

(defn register-classes [& body]
  (alter-var-root #'*classes*
    #(reduce
      (fn [classes [class-name indices]]
        (assoc classes (name class-name)
        (into #{} (map name indices))))
        %
        (partition 2 body))))
    
; Relationships

(defn register-relations [& relations]
  (alter-var-root #'*named-relations*
    (fn [named-relations]
      (reduce
        (fn [relations [outgoing incoming]]
          (if incoming
            (merge relations {outgoing {:direction Direction/OUTGOING :type outgoing}
                              incoming {:direction Direction/INCOMING :type outgoing}})
            relations))
        named-relations
        relations))))
    
; Search

(defn find-nodes
  ([index search]
    (or
      (map #(Neo-Node. %) (seq (.getNodes *lucene* (name index) search)))
      '()))
  ([class index search]
    (find-nodes (str (name class) "__" (name index)) search)))
    
; Constructor
    
(defn ^Neo-Node node!
  ([] (node! {}))
  ([class properties]
    (node! (merge properties {:__CLASS (name class)})))
  ([properties]
    (Neo-Node. (do-tx
      (setProperties!
        (.createNode *neo*)
        properties)))))