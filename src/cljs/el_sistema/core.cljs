(ns el-sistema.core
  (:require [clojure.browser.repl :as repl]
            [matchbox.core :as m]
            [quil.core :as q :include-macros true]
            [el-sistema.logic :as logic]
            [el-sistema.sun :as sun]
            [el-sistema.simulation :as simulation]))

(enable-console-print!)
(def garden (atom nil))

;; (defonce conn
;;   (repl/connect "http://localhost:9000/repl"))

;; (def root (m/connect "https://el-sistema.firebaseio.com"))

;; (m/auth-anon root)

;; (m/listen-children
;;   root [:users :mike :friends]
;;   (fn [[event-type data]] (prn event-type data)))

;; (def mikes-friends (m/get-in root [:users :mike :friends]))
;; (m/reset! mikes-friends [{:name "Kid A"} {:name "Kid B"}])
;; (m/conj! mikes-friends {:name "Jean"})

;; (m/deref
;;   mikes-friends
;;   (fn [key value]
;;     (m/reset-in! root [:users :mike :num-friends]
;;                  (count value))))

(def garden-width 600)

(def genome-1 "(genome
                 (rule (< length 50)  => (grow 10))
                 (rule (>= length 50) => (branch +10 -10))
                    )")
(def genome-2 "(genome
                                            (rule (< length 200) => (grow 3))
                                            (rule (> length 200) => (branch + 5)))")




(defn print-plant-string [plant] (println "PLANT:" (logic/plant->string plant)) plant)
(defn print-plant-energy [plant] (println "ENERGY:" (:energy plant)) plant)
(defn print-plant-segments [plant] (println "SEGMENTS " (logic/plant->segs 100 plant)) plant)

(defn segs [plant]
  ;; (println plant)
  (->> plant
       ;(print-plant-string)
       ;; (print-plant-energy)
       ;; (print-plant-segments)
       (logic/plant->segs (:x plant))))

(def fps 30)
(def max-gen (* fps 60))

(defn finished?
  ([] (finished? @garden))
  ([garden] (>= (:generation garden) max-gen)))

(defn final-energy [plants]
  (map (fn [{:keys [energy total-energy] :as plant}]
         (-> plant
             (assoc :total-energy (- total-energy energy))
             (assoc :energy 0)))
       plants
       ))

(defn final-energy-percentage [total-energy plants]
  (map (fn [plant] (assoc plant :total-energy (/ (:total-energy plant) total-energy))) plants))

(defn compute-result [garden]
  (let [gardenp (update garden :plants final-energy)
        total-energy (reduce + (map :total-energy (:plants gardenp)))]
    (update gardenp :plants (partial final-energy-percentage total-energy))))

(defn scores []
  (clj->js (map (fn [{:keys [total-energy energy]}]
                 (- total-energy energy))
               (:plants @garden))))

(defn draw []
  (q/background 100)
  (q/stroke-float 0)
  ;; (println  (:plants @garden))
  ;; (println "count" (count (:plants @garden) ))
  (let [plants-segs (map segs (:plants @garden))
        ;; _ (println "num plant: " (count  (:plants @garden)))
        ;; _ (println "num segs" (count  plants-segs))
        absorbs (sun/draw [300 400] plants-segs garden-width 400)
        ;; _ (println "absorbs: " absorbs)
        next-garden (simulation/evolve-garden @garden absorbs)]
    ;; (println next-garden)
    (if (finished? next-garden)
      (reset! garden (compute-result next-garden))
      (reset! garden next-garden))))

(defn setup []
  (q/smooth 0)
  (q/frame-rate fps)
  (q/color-mode :rgb)
  (q/set-state! :light (q/create-graphics garden-width 400))
  (draw))


(q/defsketch hello
      :setup setup
      :draw draw
      :host "tree"
      :size [garden-width 400]
      )

(defn run ^:export [g1 g2]
  (let [g1 (logic/parse-genome-string g1)
        g2 (logic/parse-genome-string g2)]
    (reset! garden (simulation/make-garden garden-width [g1 g2]))
    ))

(defn run-sim []
  (run genome-1 genome-2))
