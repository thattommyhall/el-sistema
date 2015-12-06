(ns el-sistema.simulation
  (:require [el-sistema.logic :as logic]
            [el-sistema.sun :as sun]))

(defrecord Garden [plants size])

(defn seed-garden [garden genomes]
  (let [plant-area (/ (:size garden) (count genomes))
        ]
    ;; (println plant-area)
    (assoc garden
           :plants
           [(assoc (logic/seed (first genomes)) :x 200)
            (assoc (logic/seed (second genomes)) :x 400)
            ]
           #_(loop [x 0
                  genomes genomes
                  plants []]
             (if (seq genomes)
               (let [new-x (+ x plant-area)
                     plant-x (/ new-x 2)
                     new-plant
                     new-plant (assoc new-plant :x plant-x)]
                 (recur new-x
                        (rest genomes)
                        (cons new-plant plants)))
               (assoc garden :plants (reverse plants))))
           )))

(defn make-garden [size genomes]
  (-> (map->Garden {:size size})
      (seed-garden genomes)))

(defn update-absorbs [plants absorbs]
  (let [total-energy (reduce + absorbs)]
    (map  (fn [plant absorb] (assoc plant :absorb (/ absorb total-energy))) plants absorbs)))

(defn compute-plant-energy-increments [{:keys [plants]} total-energy]
  (map #(* (:absorb %) total-energy ) plants))

(defn evolve-garden [{:keys [plants] :as garden} absorbs]
  (println "plants to e-g: " (count plants))
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
