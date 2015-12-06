(ns el-sistema.simulation
  (:require [el-sistema.logic :as logic]
            [el-sistema.sun :as sun]))

(defrecord Garden [plants size])

(defn seed-garden [garden genomes]
  (let [plant-area (/ (:size garden) (count genomes))]
    (assoc garden
           :plants
           (map (fn [genome x-val]
                  (assoc (logic/seed genome) :x x-val))
                genomes
                (iterate #(+ plant-area %) (/ plant-area 2))))))

(defn make-garden [size genomes]
  (-> (map->Garden {:size size})
      (seed-garden genomes)))

(defn update-absorbs [plants absorbs]
  (let [total-energy (reduce + absorbs)]
    (map  (fn [plant absorb] (assoc plant :absorb (/ absorb total-energy))) plants absorbs)))

(defn compute-plant-energy-increments [{:keys [plants]} total-energy]
  (map #(* (:absorb %) total-energy ) plants))

(defn evolve-garden [{:keys [plants] :as garden} absorbs]
  (let [next-energy-amount 100
        ;; _ (println next-energy-amount)
        garden (assoc garden :plants (update-absorbs plants absorbs))
        ;; _ (println "garden" (map :absorb (:plants garden)))
        energy-increments (compute-plant-energy-increments garden next-energy-amount)
        _ (println "energy-incs: "(doall energy-increments))
        new-plants (map logic/evolve-plant plants energy-increments)
        ;; _ (println new-plants)
        ]

    (assoc garden :plants new-plants)))
