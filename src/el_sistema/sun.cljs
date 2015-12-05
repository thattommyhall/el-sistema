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

(def all-lines
  (vec (concat lines edges)))

(defn line-intersection [[p0_x p0_y p1_x p1_y] [p2_x p2_y p3_x p3_y]]
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
      (when (and (> s 0) (< s 1) (> t 0) (< t 1))
        [s t ix iy]))))

(def intersections
  (vec
   (filter identity
    (for [line0 all-lines
          line1 all-lines]
      (line-intersection line0 line1)))))

(defn ray-intersection [[p0_x p0_y p1_x p1_y] [p2_x p2_y p3_x p3_y]]
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
     (if-let [[s t ix iy] (ray-intersection ray line)]
       (if (< t t-min)
         [s t ix iy]
         [s-min t-min ix-min iy-min])
       [s-min t-min ix-min iy-min]))
   [0 js/Number.POSITIVE_INFINITY (ray 2) (ray 3)]
   lines))

(defn impacts [[sx sy] lines intersections]
  (vec
   (concat
    (for [[x0 y0 x1 y1] lines]
      (let [[_ _ ix iy] (closest-intersection [sx sy x0 y0] all-lines)]
        [ix iy]))
    (for [[x0 y0 x1 y1] lines]
      (let [[_ _ ix iy] (closest-intersection [sx sy x1 y1] all-lines)]
        [ix iy]))
    (for [[x y] intersections]
      (let [[_ _ ix iy] (closest-intersection [sx sy x y] all-lines)]
        [ix iy])))))

(defn compare-by-angle [[sx sy] [ax ay] [bx by]]
  (cond
   (and (>= (- ax sx) 0) (< (- bx sx) 0)) true
   (and (< (- ax sx) 0) (>= (- bx sx) 0)) false
   (and (== (- ax sx) 0) (== (- bx sx) 0)) (if (or (>= (- ay sy) 0) (>= (- by sy) 0))
                                             (> ay by)
                                             (> by ay))
   true (let [det (-
                   (* (- ax sx) (- by sy))
                   (* (- bx sx) (- ay sy)))]
          (cond
           (< det 0) true
           (> det 0) false
           true (let [d1 (+
                          (* (- ax sx) (- ax sx))
                          (* (- ay sy) (- ay sy)))
                      d2 (+
                          (* (- bx sx) (- bx sx))
                          (* (- by sy) (- by sy)))]
                  (> d1 d2))))))

(defn sort-by-angle [centre points]
  (vec
   (sort-by identity (fn [px py] (compare-by-angle centre px py)) points)))

(defn draw []
  (let [sun [(q/mouse-x) (q/mouse-y)]
        impacts (impacts sun lines intersections)
        sorted-impacts (sort-by-angle sun impacts)]
    (println "drawing at" (q/current-frame-rate))
    (println impacts)
    (println sorted-impacts)
    (q/background 255)
    (q/fill 255 255 0)
    (q/stroke 255 255 0)
    (q/ellipse (sun 0) (sun 1) 10 10)
    (doseq [[ix iy] sorted-impacts]
      (q/line (sun 0) (sun 1) ix iy))
    (q/stroke 0 0 0)
    (doseq [[ix iy] (take 1 sorted-impacts)]
      (q/line (sun 0) (sun 1) ix iy))
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