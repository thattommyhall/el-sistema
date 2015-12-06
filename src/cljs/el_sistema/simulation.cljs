(ns el-sistema.simulation
  (:require [el-sistema.logic :as logic]
            [el-sistema.sun :as sun]))

(defrecord Garden [plants size])

(defn seed-garden [garden genomes]
  (let [plant-area (/ (:size garden) (count genomes))]
    (assoc garden
           :plant (loop [x 0
                         genomes genomes
                         plants []]
                    (if (seq genomes)
                      (let [new-x (+ x plant-area)
                            plant-x (/ new-x 2)
                            new-plant (logic/seed genome)
                            new-plant (assoc new-plant :x plant-x)]
                        (recur new-x
                               (rest genomes)
                               (cons new-plant plants)))
                      (assoc garden :plants (reverse plants)))))))


(defn make-garden [size genomes]
  (-> (map->Garden {:size size})
      (seed-garden genomes)))

(defn update-absorbs [plants absorbs]
  (let [total-energy (reduce + absorbs)]
    (map  (fn [plant absorb] (assoc plant :absorb (/ absorb total-energy))) plants absorbs)))

(defn compute-plant-energy-increments [{:keys [plants]} total-energy] (map #(* (:absorb %) total-energy)))

(defn evolve-garden [{:keys [plants] :as garden} absorbs]
  (let [next-energy-amount 100
        garden (assoc garden :plants (update-absorbs plants absorbs))
        energy-increments (compute-plant-energy-increments garden next-energy-amount)]
    (assoc garden :plants (map logic/evolve-plant (:plants garden) energy-increments))))
