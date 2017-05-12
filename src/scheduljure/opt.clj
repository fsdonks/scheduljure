;;This is an interpretation of the scheduler
;;that uses a dumb stochastic hill-climber to
;;compute an approximately optimal schedule
;;according to a dumb goal program.
(ns scheduljure.opt)
;;utils

;;Collect a map of {k [idx1 idx2 ...]} from a
;;collection of [k1 k2 k3], where idx corresponds
;;to the index in the original collection xs where
;;k occured.
(defn indexed-samples [xs]
  (transduce (map-indexed vector)
             (completing (fn [acc [idx nm]]
                           (let [xs (get acc nm [])]
                             (assoc acc nm (conj xs idx)))))
             {}
             xs))

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

(def week->idx (into {} (map-indexed #(vector %2 %1) weeks)))
(def availability (into {} (for [[w s] unavailables]
                             [(week->idx w) s])))

;;given a vector of names, a map of [week -> set unvailable-names]
;;and and a wk :: int, provides a screen collection of
;;feasible names.
(defn choices
  [names uns wk]
  (if-let [invalid (get uns wk)]
    (filter (complement invalid) names)
    names))

;;Bad weeks for a user's schedule are weeks in which
;;we have multiple checks scheduled within the span of a
;;month (i.e. 4 weeks).  The degre of badness is
;;measured by the square of the distance of the
;;week-index-difference from the goal of 4.  So,
;;"adjacent" i.e. closer weeks are punished more.
(defn bad-weeks [xs]
  (->> xs
       (reduce (fn [[acc p] n]                 
                 (if-let [res (and p (- n p))]
                   (if (< res 4)
                     [(+ acc (Math/pow (- 4 res) 2)) n]
                     [acc n])
                   [acc n])) [0 nil])
       (first)))

;;Our cost function is just the sum of bad weeks for a solution,
;;which is the sum of squared distance from the per-user
;;goal of having 1 week scheduled every 4 weeks.
(defn cost  [s]
  (->> (indexed-samples s)
       (reduce-kv (fn [acc nm wks]
                    (+ acc (bad-weeks wks))) 0)))

;;We dumbly alter our solution by picking a random week
;;and altering the decision according to our constraints.
(defn flip! [s wk->choices]
  (let [idx  (rand-int (count s))
        old  (nth s idx)
        nebs (wk->choices idx)]
    (assoc s idx (rand-nth nebs))))

;;Generate a random initial solution to start flipping.
(defn random-solution [names uns wks]
  (->> (count wks)
       (range)
       (mapv (comp rand-nth #(choices names uns %)))))

;;Dumb stochastic hill-climber that only accepts improving
;;solutions.
(defn schedule!
  [nms uns wks & {:keys [max-it max-time]
                  :or {max-it 10000}}]
  (let [wk->choices #(choices names uns %)]
    (loop [idx      0
           sol      (random-solution names uns wks)
           solcost  (cost sol)]
      (if (or (zero? solcost)
              (>= idx max-it))
          {:cost solcost :sol sol}
          (let [nxt     (flip! s wk->choices)
                cnext   (cost    nxt)
                accept? (< cnext solcost)]       
            (recur (unchecked-inc  idx)
                   (if accept? nxt sol)
                   (if accept? cnext solcost)))))))

;;Exercise for the reader:
;;Do it using simulated annealing :)
