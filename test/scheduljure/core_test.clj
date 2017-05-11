(ns scheduljure.core-test
  (:require [scheduljure.core :as cr]
            [scheduljure.permutations :as p])
  (:use clojure.test))

(deftest addition
  (is (= 4 (+ 2 2)))
  (is (= 7 (+ 3 4))))

;please note that an element of the permutation method won't necessarily match the method
;in core, so the code here in core-test is useless :)

(def ^:dynamic *path* "K:\\Divisions\\FS\\Admin\\Security Check\\Automated\\05-08-2017\\")

(defn vec-unavails [unavail-map weeks name->id]
  (map (fn [week] (map name->id (get unavail-map week))) weeks))

(defn get-names [path]
  (read-string (slurp  (str path "names.txt"))))

(defn make-id-map [path]
  (let  [names (get-names path)
         ids (p/make-ids names)]
    (zipmap names ids)))

(defn get-equivalent [path]
  (let [idmap (make-id-map *path*)]
    (-> (vec-unavails (cr/get-unavailability *path*)
                            (read-string (slurp (str *path* "weeks.txt")))
                            idmap)
              (p/find-all-schedules (p/make-ids (get-names *path*)))
              )))

(deftest check-algorithm
  (let [[_ _ _ roster] (cr/input->roster *path*)
        permutations (get-equivalent *path*)
        rids (map (make-id-map *path*) roster)]
    (is (= (first permutations) rids))
    (is (some #{rids} permutations))))


