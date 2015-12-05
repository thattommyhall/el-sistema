(ns el-sistema.sun
  (:require [quil.core :as q :include-macros true])
  )

(enable-console-print!)

(defn lines []
  (vec
   (for [i (range 10)]
    [(rand-int 500) (rand-int 500) (rand-int 500) (rand-int 500)])))

(defn sun []
  [(rand-int 500) (rand-int 500)])

(defn draw []
  (let [lines (lines)
        sun (sun)]
    (println "drawing at" (q/current-frame-rate))
    (q/background 255)
    (q/fill 255 255 0)
    (q/stroke 255 255 0)
    (q/ellipse (sun 0) (sun 1) 10 10)
    (q/stroke 0 255 0)
    (doseq [[x0 y0 x1 y1] lines]
      (q/line x0 y0 x1 y1))))

(defn setup []
  (q/frame-rate 1)
  (q/color-mode :rgb))

(q/defsketch sun-sketch
  :setup setup
  :size [500 500]
  :draw draw
  :host "screen")