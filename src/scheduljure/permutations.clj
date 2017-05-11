(ns scheduljure.permutations (:require [clojure.math.combinatorics :as c]))

(def checkers ["Rick" "Tom" "Craig" "Shawnette" "Heather" "Greg"])

(defn make-ids [checkers]
  (range 1 (inc (count checkers))))

(def checker-ids (make-ids checkers))

(def navail [[4 5]   ;<-- checkers 4 and 5 aren't available on week 1
             [1 5]   ;<-- checkers 1 and 5 aren't available on week 2
             [1 2 6] ; ... and so on ...
             [2 3 1]
             [5 2]
             [1 2 3 4 6]])

(defn member? [elem coll]
  (and (some #{elem} coll) true))

;; don't know if this version (with reduced) is faster.
(defn schedule-ok? [navail schedule]
  (reduce (fn [flag [navail-this-week checker]]
            (or (not (member? checker navail-this-week))
                (reduced nil)))
          true
          (map #(-> [%1 %2]) navail schedule)))

(defn schedule-ok? [navail schedule]
  (reduce (fn [flag [navail-this-week checker]]
            (and flag
                 (not (member? checker navail-this-week))))
          true
          (map #(-> [%1 %2]) navail schedule)))

(defn find-all-schedules [navail checkers]
  (filter (partial schedule-ok? navail)
          (c/permutations checkers)))

(comment
  ;; punchline:
  (time (find-all-schedules navail checker-ids))
  ;;-> ([1 2 3 4 6 5] [1 2 3 6 4 5] [1 2 4 6 3 5] [2 3 4 6 1 5] [2 4 3 6 1 5] [2 6 3 4 1 5] [3 2 4 6 1 5] [6 2 3 4 1 5])
)
;; java -cp "C:/Users/rick.hanson/.m2/repository/org/clojure/math.combinatorics/0.1.4/math.combinatorics-0.1.4.jar;C:/Users/rick.hanson/.m2/repository/org/clojure/clojure/1.8.0/clojure-1.8.0.jar" clojure.main
;; user=> (load-file "security-duty-schedule.clj")
;; nil
;; user=> checker-ids
;; (1 2 3 4 5 6)
;; user=> navail
;; [[4 5] [1 5] [1 2 6] [2 3 1] [5 2] [1 2 3 4 6]]
;; user=> (find-all-schedules navail checker-ids)
;; ([1 2 3 4 6 5] [1 2 3 6 4 5] [1 2 4 6 3 5] [2 3 4 6 1 5] [2 4 3 6 1 5] [2 6 3 4 1 5] [3 2 4 6 1 5] [6 2 3 4 1 5])

