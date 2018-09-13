;;Yet another way to schedule, this time using the network
;;flow libraries in spork.cljgraph.
(ns scheduljure.net
  (:require [spork.cljgraph.flow :as flow]))

;1) Vector of names ["Rick" "Tom" "Craig"]
;2) Map of unavailable days {"04/19/17" #{"Rick"} "04/26/17" #{"Rick" "Craig"}}
;3) Weeks ["04/12/17" "04/19/17" "04/26/17"]

;Outputs:
;1) New stack ["Craig" "Rick" "Tom"]
;2) New weeks ["05/03/17" "05/10/17" "05/17/17"]
;3) Roster [["04/12/17" "Rick"] ["04/19/17" "Tom"] ["04/26/17" "Tom"]]
                                   
;;given a schedule of the form...
;;[0 1 2 3 4 5 6 7 8 9 0]
;;Our goal is to assign names to slots (i.e. weeks)
;;such that:
;;  We never assign a name to an unavailable slot.
;;  We maximize the average distance between adjacent security checks.

(def names ["Rick" "Tom" "Craig"])
(def all-names (set names))

(def unavailables {"04-10-2017" #{"Rick"}
                   "04-17-2017" #{"Rick" "Craig"}})
(def weeks ["04-03-2017" "04-10-2017" "04-17-2017" "04-24-2017"])

;;let's build a mincost-flow problem.
;;our goal is to push flow to each week.
;;edges leading to each week have capacity 1, cost 0.

;;we want to make multiple assignments in a month
;;from the same person cost something.

;;So, for each person in a month, we create edges 1..4
;;that have increasing cost.

;;In the graph below, we draw out nodes the
;;network connected by undirected edges.
;;Edges are either labeled with cost:capacity,
;;or inferred to have 0:infinite cost and
;;capacity if unlabeled.

;;nodes annotated with (...) indicate
;;a duplication (i.e. a reference to
;;an earlier node), typically to save
;;space visually.

;; --------person1____________________
;; |          \        \      \       \
;; |           3-11   2-11    1-11    1-11  
;; |             \12:1   |9:1   /3:1   /0:1
;; |              person1-in-month1
;; |                   \____ w1-----\  
;; S                    \___ w2----- ___________  
;; |                     \__ w3-----            \ 
;; |                      \__w4-----/            \
;; --------person2____________________            \  
;;            \        \      \       \            T
;;             3-11   2-11    1-11    1-11        /
;;               \12:1   |9:1   /3:1   /0:1      /
;;                person2-in-month1             /
;;                     \____ (w1)-----\        /
;;                      \___ (w2)----- _______/  
;;                       \__ (w3)-----      
;;                        \__(w4)-----/

;;Our goal is to push maximum flow from S to T
;;such that the cost of flow is minimized, and
;;our solution obeys some basic constraints.

;;Formally as an LP,

;;min Z = c.x 

;;s.t. 
;;;Flow must be within capacity..
;;for i,j in arcs
;;  capacity(i,j) - x_i,j >= 0

;;;Conservation of Flow... 
;;for n in nodes, snk in sinks(n), src in sources(n)
;;  sum(x_src,n) = sum(x_n,snk)

;;;Required flow...
;;for w in weeks,
;; sum(x_w,T) = cardinality(weeks) 

;;where
;;arcs = Set : (src,snk,cost,capacity)
;;nodes = srcs U snks, where
;;  srcs = {src | (src,_,_,_) <- arcs}
;;  snks = {snk | (_,snk,_,_) <- arcs}
;;weeks intersects nodes,
;;T in nodes
;;capacity(i,j) = cap, where {cap | (i,j,_,cap) <- arcs}
;;cost(i,j) = cost, where {cost | (i,j,cost,_) <- arcs}
;;c_i,j = cost(i,j)


;;Rather than solve it as an lp, we can use
;;discrete methods, namely graph algorithms and
;;and augmented directed graph, which forms a
;;capacitated flow network.  The simplest
;;of these algorithms is to compute a
;;residual network - that is a network of
;;uncapacitated edges, i.e. "available
;;routes", and use our library of SSSP
;;algorithms (ala priority-first-search/Djikstra),
;;to find a path from S to T through the
;;residual network, augmenting flow along
;;the way.  Doing so repeatedly, we
;;arrive at both a maxflow at mincost when
;;no further mincost paths exist in the residual
;;network.

;;We need only build the network visualized
;;above.  Thankfully, that boils down to
;;using functions to build the arcs, then
;;concatenating all the capacitated arcs
;;together into a network.

(defn amount-nodes
  ([name mnode bound]
   (apply concat 
          (for [c (range bound)]
            (let [amt  (str name "-"
                            (case c
                              0 "Once"
                              1 "Twice"
                              2 "Thrice"
                              3 "Fourthington"))]
              [[name amt (* c c) 9999]
               [amt mnode 0 1]]))))
  ([name mnode] (amount-nodes name mnode 4)))
  
;;Let's build the network!
(defn month-arcs [names month-weeks available?]
  (->> (for [n     names
             [month ws] month-weeks]
         (let [mnode (str n "-in-" month)]
           (into (amount-nodes n mnode)
             (for [w     ws
                   :when (available? n w)]
               [mnode w 0 1]))))
       (apply concat)))

;;note: due to some peculiarities in using a treeset
;;for the most recent priority q implementation,
;;we have to ensure that both node-weights and
;;node-labels are comparable, i.e. can be inputs
;;for (compare ...).  That is, we currently
;;can't mix-and-match keywords and strings for
;;node-labels, which is what was happening.
;;Looking into fixing this going forward (minor
;;annoyance).
(defn network-arcs [names weeks unavailables]
  (let [month-weeks (partition-all 4 weeks)
        months (mapv #(str "Month" %)
                     (range (count month-weeks)))
        available? (fn [person week]
                     (not (when-let [res (get unavailables week)]
                            (res person))))]
    (concat (month-arcs names (map vector months month-weeks) available?)
      (for [n names] ["s" n 0 9999])
      (for [w weeks] [w "t" 0 1]))))
            

(defn build-net [names weeks availability]
  (->> (network-arcs names weeks availability)
       (flow/conj-cap-arcs flow/empty-network)))

(defn compute-flow [names weeks availability]
  (let [net (build-net names weeks availability)
        res (-> net
                (flow/mincost-flow "s" "t")
                :active)
        parent-of-parent (fn [nd]
                           (->>  nd
                                 (spork.cljgraph.core/sources net)
                                 (first)
                                 (spork.cljgraph.core/sources net)
                                 (first)))
                                
        week? (set weeks)]
    (for [[[person-in-month w] flow] res
          :when (week? w)]
      [w (parent-of-parent person-in-month)])))


;;another formulation that may be better:
;;The flaw in the original formulation is that we can end up with
;;arbitrary sucker cycles: If we have fewer people than
;;weeks in a month, one person will pull more than 1
;;shift per month, and there's nothing preventing
;;this from repeating throughout the solution.

;;We can reformulate to capture the cost of flow
;;as the delay between assignments.
;;More delay -> cheaper flow.

;;For each p names
;;for from in (p,from)
;;    to   in (p,to)
;;    forall from in weeks
;;           to in weeks, where to > from
;;cost[(p, from) (p,to)] = (to - from)^2
;;capacity[(p,from),total] = 1
;;capacity(s, p) = inf
;;cost(s,p) = 0
;;for each p in names
;;    for each w in weeks
;;capacity[(p, (p,w))] = 1
;;cost    [(p, (p,w))] = 0

;;note: due to some peculiarities in using a treeset
;;for the most recent priority q implementation,
;;we have to ensure that both node-weights and
;;node-labels are comparable, i.e. can be inputs
;;for (compare ...).  That is, we currently
;;can't mix-and-match keywords and strings for
;;node-labels, which is what was happening.
;;Looking into fixing this going forward (minor
;;annoyance).
(defn weekly-arcs [names weeks unavailables]
  (let [available? (fn [nm week]
                     (not (when-let [res (get unavailables week)]
                            (res nm))))
        tmax (reduce max weeks)]
    (apply concat
      (for [n names]
        (let [aweeks (filter #(available? n %) weeks)]        
          (for [l aweeks
                r (rest aweeks)
                :when (> r l)]
            (let [from (str n "-" l)
                  to   (str n "-" r)
                  cost (- tmax (- r l))]
              [from to cost 1])))))))

(defn network-arcs2 [names weeks unavailables]
  (let [warks  (weekly-arcs names weeks unavailables)
        nodes  (reduce (fn [acc [l r & rest]] 
                         (conj acc l r)) #{} warks)
        tfinal (reduce max weeks)
        k      (count names)]
    (concat
      (for [n nodes]
        ["s" n 0 1])
      warks
      (for [n nodes]
        [n "total" 9999 1])
      [["total" "t" 0 k]])))

(defn build-net2 [names weeks availability]
  (->> (network-arcs2 names weeks availability)
       (flow/conj-cap-arcs flow/empty-network)))
      
(defn compute-flow2 [names weeks availability]
  (let [net (build-net2 names weeks availability)
        res (-> net
                (flow/mincost-flow "s" "t")
                :active)]
    res))
    
    

;;scheduljure.net> (sort-by first (compute-flow names weeks unavailables))
;;(["04-03-2017" "Rick"] ["04-10-2017" "Craig"] ["04-17-2017" "Tom"] ["04-24-2017" "Rick"])

(comment
  (sort-by first     (compute-flow names weeks unavailables))
  (def lots-of-weeks (mapv str (range 100)))
  (defn random-unavailables [nms wks] 
    (->> (for [i wks]
           [i (when-let [res (seq (for [n nms          
                                        :when (> (rand) 0.98)]
                                    n))]
                (set res))])
         (filter (comp seq second))
         (into {})))
  
  (def lots-of-unavailables 
    (random-unavailables names lots-of-weeks))
  
  (defn verify [unavailables weekly-assignments]
    (filter identity
      (for [[w nm] weekly-assignments]
        (when-let [un (unavailables w)]
          (when (un nm)
            {:invalid! [w nm]})))))
  
  (def res  
    (->> (compute-flow names 
               lots-of-weeks
               lots-of-unavailables)
         (sort-by (comp clojure.edn/read-string first)))))  
            
            
      
       
          
        



                                   





