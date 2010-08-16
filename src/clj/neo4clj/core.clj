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
                              PropertyEntry
                              TransactionData)
     (org.neo4j.index.lucene LuceneFulltextIndexService)
     (org.neo4j.helpers Predicate)))
     
; Protocols

(defprotocol PElement
  (alter! [this f])
  (delete-aux! [this]))

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
     
; Neo Database
        
(def ^EmbeddedGraphDatabase *neo* nil)
(def ^LuceneFulltextIndexService *lucene* nil)
(def *named-relations* {})
(def *classes* {})
(def *indices* #{})

(declare attach-index-handler)

(defn start
  "Start a neo4j instance from the given database path, and bind to *neo*"
  [path]
  (do
    (alter-var-root #'*neo* (fn [_] (EmbeddedGraphDatabase. path)))
    (alter-var-root #'*lucene* (fn [_] (LuceneFulltextIndexService. *neo*)))
    (attach-index-handler)))
  
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
  
(defmacro do-tx
  "Wraps the contents inside a transaction and implicit do."
  [& body]
  `(if *tx* (do ~@body)
    (binding [*tx* (.beginTx *neo*)]
    (try
      (let [val# (do ~@body)]
        (success)
        val#)
      (finally
        (finish))))))
     
; PropertyContainer extensions.
     
(defn boolean? [o]
  (= java.lang.Boolean (type o)))
     
(defn- object-storage-type
  "Returns best object type for storage of given object, or nil if invalid."
  [o]
  (cond
    (boolean? o) java.lang.Boolean
    (string? o)  java.lang.String
    (integer? o) java.lang.Long
    (number? o)  java.lang.Double))
    
(defn- type-convert
  "Returns a type conversion function for the given Java type."
  [type]
  (cond
    (= type java.lang.Boolean) boolean
    (= type java.lang.String)  str
    (= type java.lang.Long)    long
    (= type java.lang.Double)  double))
     
(defn array-storage-type
  "Accepts a sequence and returns a Java type for that sequence, or otherwise
  throws an exception if a valid type cannot be found."
  [arr]
  (reduce
    (fn [guess el]
      (let [el-type (object-storage-type el)]
        (cond
          (= guess el-type) guess
          (and (or (= guess java.lang.Long)
                   (= guess java.lang.Double))
               (or (= el-type java.lang.Long)
                   (= el-type java.lang.Double))) java.lang.Double
          (nil? guess) el-type
          :default (throw (Exception. "Invalid type mix.")))))
    nil
    arr))
     
(defn- seq-to-array
  "Converts a sequence into a Java array of an appropriate type for storage."
  [sequence]
  (let [arr-type (array-storage-type sequence)]
    (into-array arr-type (map (type-convert arr-type) sequence))))
     
(extend-type PropertyContainer
  PropertyWrap
  (setProperties! [this properties]
    (doseq [[key value] properties]
      (if (or (seq? value) (vector? value) (set? value))
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
  
(defn predicate
  "Returns a Predicate wrapping of f for use with TraversalDescription.filter"
  [f]
  (reify
    Predicate
      (accept [_ item] (f item))))
      
(defn pruner
  "Returns a PruneEvaluator wrapping of f for use with
  TraversalDescription.prune"
  [f]
  (reify
    PruneEvaluator
      (^boolean pruneAfter [_ ^Path path] (f path))))
  
(defn where
  "Adds a filter function f onto the traversal and returns the new traversal.
  Only one filter may be used, and a previous filter will be replaced."
  [f ^TraversalDescription traversal]
  (.filter traversal (predicate f)))
  
(defn prune
  "Adds a prune function f onto the traversal and returns the new traversal.
  The prune filter should return true when traversal should stop. Note that
  the traversal *will* return the node at which the traversal was stopped."
  [f ^TraversalDescription traversal]
  (.prune traversal (pruner f)))
  
(defn all-but-start
  "Preset call for where, which filters out the start node from a traversals
  results."
  [^TraversalDescription traversal]
  (where #(not= (.startNode %) (.endNode %)) traversal))
  
(defn get-nodes-from
  "Executes a given traversal and returns the nodes as a result."
  [^Neo-Node node ^TraversalDescription traversal]
  (map #(Neo-Node. %) (.nodes (.traverse traversal (.element node)))))
  
(defn along-mult [traversal & relations]
  (reduce (fn [^TraversalDescription traversal [type direction]]
    (.relationships traversal (Neo-RelationshipType. type) direction))
    traversal
    (partition 2 relations)))
    
(defn along
  "Adds valid relationships to be followed in the traversal. Is in addition to
  any previous valid relationships added. If none are set, then all
  relationships are valid."
  [type direction ^TraversalDescription traversal]
  (.relationships traversal (Neo-RelationshipType. type) direction))

(defn ^TraversalDescription max-depth
  "Preset prune where traversal stops at the given depth."
  [n ^TraversalDescription traversal]
  (prune #(= (.length %) n) traversal))
(defn ^TraversalDescription depth-first
  "Set the traversal to use depth first search."
  [^TraversalDescription traversal]
  (.depthFirst traversal))
(defn ^TraversalDescription breadth-first
  "Set the traversal to use breadth first search."
  [^TraversalDescription traversal]
  (.breadthFirst traversal))

(defn new-traversal
  "Returns a new traversal."
  []
  (Traversal/description))
    
(def single-level-traverse
  (->> (new-traversal)
       (depth-first)
       (max-depth 1)
       (all-but-start)))
      
(defn related-via-label
  "Returns the neighbour nodes of a node along a given relationship type and
  direction."
  [node type direction]
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

  PElement
    (alter! [_ f]
      (do-tx
        (setProperties! element (f (getProperties element)))))
    (delete-aux! [this] (.delete element))

  PNode
    (relate! [this type to] (relate! this type to {}))
    (relate! [_ type to properties]
      (Neo-Relationship. (do-tx
        (let [relation (.createRelationshipTo element
                         (.element ^Neo-Node to) (Neo-RelationshipType. type))]
          (setProperties! relation properties)))))
    (related [this type]
      (related-via-label this type Direction/BOTH))
    (related [this type direction]
      (related-via-label this type direction))

  clojure.lang.IFn
    (invoke [this type]
      (if-let [rel-type (*named-relations* type)]
        (related this (rel-type :type) (rel-type :direction))
        (related this type both))))

; Relationship

(deftype Neo-Relationship [^Relationship element]

  Object
    (equals [_ other]
      (= element (.element ^Neo-Node other)))

  clojure.lang.IDeref
    (deref [_] (getProperties element))

  PElement
    (alter! [_ f]
      (do-tx
        (setProperties! element (f (getProperties element)))))
    (delete-aux! [this] (.delete element))

  PRelationship
    (start-node [_] (Neo-Node. (.getStartNode element)))
    (end-node [_] (Neo-Node. (.getEndNode element))))
    
; Events/Indexing
    
(defn- deleted-node-classes [deleted-nodes removed-properties]
  (let [removed-nodes (into {} (map (fn [^Node node] [node {}]) deleted-nodes))]
    (reduce
      (fn [props ^PropertyEntry removal]
        (update-in props [(.entity removal)]
          assoc (.key removal) (.previouslyCommitedValue removal)))
      removed-nodes
      (filter #(contains? removed-nodes (.entity ^PropertyEntry %))
        removed-properties))))

(defn- entity-class [^Node entity]
  (if (.hasProperty entity "__CLASS") (.getProperty entity "__CLASS")))

(defn- update-index [entity key value prev-value]
  (when prev-value
    (.removeIndex *lucene* entity key prev-value))
  (.index *lucene* entity key value))
(defn- remove-index [entity key]
  (.removeIndex *lucene* entity key))
  
(defn- class-index? [class key]
  (and class (contains? (*classes* class) key)))

(defn- index-handler [^TransactionData data]
  (let [removed-properties (.removedNodeProperties data)
        removed-nodes (deleted-node-classes (.deletedNodes data)
                        removed-properties)]
    (doseq [^PropertyEntry removal removed-properties]
      (let [entity ^Node (.entity removal)
            key (.key removal)
            class (if-let [removed-node (removed-nodes entity)]
                    (removed-node "__CLASS")
                    (entity-class entity))]
        (when-let [key (cond (contains? *indices* key) key
                         (class-index? class key) (str class "__" key))]
          (remove-index entity key))))
    (doseq [^PropertyEntry assign (.assignedNodeProperties data)]
      (let [entity ^Node (.entity assign)
            key (.key assign)
            class (entity-class entity)]
        (when-let [key (cond (contains? *indices* key) key
                         (class-index? class key) (str class "__" key))]
          (update-index entity key (.value assign)
            (.previouslyCommitedValue assign)))))))

(defn- attach-index-handler []
  (.registerTransactionEventHandler *neo*
    (reify TransactionEventHandler
      (afterCommit [_ data state] nil)
      (beforeCommit [_ data] (index-handler data))
      (afterRollback [_ data state] nil))))

(defn register-indices
  "Adds indices to the defined indices to keep.
  
  Example:
    (register-indices :index :names)"
  [& indices]
  (alter-var-root #'*indices* #(into % (map name indices))))

(defn register-classes
  "Registers body as classes and indices.
  
  Example:
    (register-classes
      :Class [:index :names]
      :Node [:index])"
  [& body]
  (alter-var-root #'*classes*
    #(reduce
      (fn [classes [class-name indices]]
        (assoc classes (name class-name)
        (into #{} (map name indices))))
        %
        (partition 2 body))))
    
; Named Relationships

(defn register-relations
  "Register named relations. Should be in format [:outgoing :incoming] or
  [:both] for names.
  
  Example:
    (register-relations
      [:outgoing :incoming]
      [:knows :known-by]
      [:friends])"
  [& relations]
  (alter-var-root #'*named-relations*
    (fn [named-relations]
      (reduce
        (fn [relations [outgoing incoming]]
          (if incoming
            (merge relations
              { outgoing {:direction Direction/OUTGOING :type outgoing}
                incoming {:direction Direction/INCOMING :type outgoing} })
            relations))
        named-relations
        relations))))
    
; Search

(defn find-nodes
  "Find nodes using specified index and search string, optionally with a class
  restriction."
  ([index search]
    (or
      (map #(Neo-Node. %) (seq (.getNodes *lucene* (name index) search)))
      '()))
  ([class index search]
    (find-nodes (str (name class) "__" (name index)) search)))
    
; Constructor
    
(defn ^Neo-Node node!
  "Create a new node with the given map of properties, or blank if none
  provided. If a class is provided, then this is added to the properties as a
  __CLASS property."
  ([] (node! {}))
  ([class properties]
    (node! (merge properties {:__CLASS (name class)})))
  ([properties]
    (Neo-Node. (do-tx
      (setProperties!
        (.createNode *neo*)
        properties)))))
        
(defn delete!
  "Deletes elements arguments which are Nodes or Relationships."
  [& elements]
  (do-tx
    (doseq [element elements]
      (delete-aux! element))))