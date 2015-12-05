(ns el-sistema.sun
  (:require [quil.core :as q :include-macros true]
            [goog.string]
            [goog.string.format])
  )

(enable-console-print!)

(def lines
  (vec
   (for [i (range 10)]
    [(rand-int 500) (rand-int 500) (rand-int 500) (rand-int 500)])))

(def edges
  [[0 0 0 500]
   [0 500 500 500]
   [500 500 500 0]
   [500 0 0 0]])

(defn intersection [[p0_x p0_y p1_x p1_y] [p2_x p2_y p3_x p3_y]]
  (let [s1_x (- p1_x p0_x)
        s1_y (- p1_y p0_y)
        s2_x (- p3_x p2_x)
        s2_y (- p3_y p2_y)
        d (+
            (* (- s2_x) s1_y)
            (* s1_x s2_y))
        s (/
           (+
            (* (- s1_y) (- p0_x p2_x))
            (* s1_x (- p0_y p2_y)))
           d)
        t (/
           (+
            (* s2_x (- p0_y p2_y))
            (* (- s2_y) (- p0_x p2_x)))
           d)]
    (let [ix (+ p0_x (* t s1_x))
          iy (+ p0_y (* t s1_y))]
;;       (q/fill 0)
;;       (q/text (goog.string/format "%.6f %.6f" s t) ix iy (+ ix 100) (+ iy 100))
      (when (and (> s 0) (< s 1) (> t 0))
        [s t ix iy]))))


(defn closest-intersection [ray lines]
  (reduce
   (fn [[s-min t-min ix-min iy-min] line]
     (if-let [[s t ix iy] (intersection ray line)]
       (if (< t t-min)
         [s t ix iy]
         [s-min t-min ix-min iy-min])
       [s-min t-min ix-min iy-min]))
   [0 js/Number.POSITIVE_INFINITY (ray 2) (ray 3)]
   lines))

(defn draw-ray [[sx sy] px py]
  (let [[s t ix iy] (closest-intersection [sx sy px py] (vec (concat lines edges)))]
    (q/line sx sy ix iy)))

(defn draw []
  (let [sun [(q/mouse-x) (q/mouse-y)]]
    (println "drawing at" (q/current-frame-rate))
    (q/background 255)
    (q/fill 255 255 0)
    (q/stroke 255 255 0)
    (q/ellipse (sun 0) (sun 1) 10 10)
    (doseq [[x0 y0 x1 y1] lines]
      (draw-ray sun x0 y0)
      (draw-ray sun x1 y1))
    (q/stroke 0 0 0)
    (doseq [[x0 y0 x1 y1] edges]
      (q/line x0 y0 x1 y1))
    (q/stroke 0 255 0)
    (doseq [[x0 y0 x1 y1] lines]
      (q/line x0 y0 x1 y1))))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb))

(q/defsketch sketch
  :setup setup
  :size [501 501]
  :draw draw
  :host "screen")