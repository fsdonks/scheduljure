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

(def week->idx    (into {} (map-indexed #(vector %2 %1) weeks)))
(def availability (into {} (for [[w s] unavailables]
                             [(week->idx w) s])))

;;let's build a mincost-flow problem.
;;our goal is to push flow to each week.
;;edges leading to each week have capacity 1, cost 0.

;;we want to make multiple assignments in a month
;;from the same person cost something.

;;So, for each person in a month, we create edges 1..4
;;that have increasing cost.

;; --------P1_______________
;; |          \        \    \
;; |           3-11   2-11  1-11
;; |             \9     |6     /0
;; |                 p1m1
;; |              /   | \   \
;; s          w1   w2   w3  w4   [weeks, sinks] --->t
;; |           \    \   |   /        
;; |                 p2m1
;; |            /9     |6    \0
;; |          3-11   2-11  1-11
;; |          /       /      /
;; |-------P2----------------

(defn amount-nodes [name mnode]
  (apply concat 
         (for [c (range 3)]
           (let [amt  (str name "-"
                          (case c 0 "Once" 1 "Twice" 2 "Thrice"))]
             [[name amt 0 1]
              [amt mnode (* c c) 1]]))))
  
;;Let's build the network!
(defn month-arcs [names months ws available?]
  (->> (for [n     names
             month months]
         (let [mnode (str n "-in-" month)]
           (into
            (amount-nodes n mnode)
            (for [w     ws
                  :when (available? n w)]
              [mnode w 0 1]
              ))))
       (apply concat)))

(defn network-arcs [names weeks availables]
  (let [months (mapv #(str "Month" %)
                     (range (count (partition-all 4 weeks))))
        available? (fn [person week]
                     (not (when-let [res (get unavailables week)]
                            (res person))))]
    (concat (month-arcs names months weeks available?)
            (for [n names]
              [:s n 0 9999])
            (for [w weeks]
              [w :t 0 9999])
            )))


(defn build-net [names weeks availability]
  (->> (network-arcs names weeks availability)
       (flow/conj-cap-arcs flow/empty-network)))

#_(defn mf-test! [& {:keys [n] :or {n 100000}}]
  (time (dotimes [i n]
          (mincost-flow (transient-network the-net) :s :t))))

          
          
          
          
        



                                   





