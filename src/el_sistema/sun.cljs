(ns el-sistema.sun
  (:require [quil.core :as q :include-macros true]
            [goog.string]
            [goog.string.format])
  )

(enable-console-print!)

(def num_ids 10)

(def lines
  (vec
   (for [i (range 10)]
    [i (rand-int 500) (rand-int 500) (rand-int 500) (rand-int 500)])))

(def edges
  [[-1 0 0 0 500]
   [-1 0 500 500 500]
   [-1 500 500 500 0]
   [-1 500 0 0 0]])

(def all-lines
  (vec (concat lines edges)))

(defn line-intersection [[_ p0_x p0_y p1_x p1_y] [_ p2_x p2_y p3_x p3_y]]
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

(defn ray-intersection [[p0_x p0_y p1_x p1_y] [_ p2_x p2_y p3_x p3_y]]
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
   (fn [[id-min s-min t-min ix-min iy-min] line]
     (if-let [[s t ix iy] (ray-intersection ray line)]
       (if (< t t-min)
         [(line 0) s t ix iy]
         [id-min s-min t-min ix-min iy-min])
       [id-min s-min t-min ix-min iy-min]))
   [-1 0 js/Number.POSITIVE_INFINITY (ray 2) (ray 3)]
   lines))

(defn rotate [angle [x y] [sx sy]]
  (let [a (q/cos angle)
        b (- (q/sin angle))
        c (- b)
        d a
        vx (- x sx)
        vy (- y sy)
        nx (+ (* a vx) (* b vy))
        ny (+ (* c vx) (* d vy))]
    [(+ sx nx) (+ sy ny)]))

(defn impacts [[sx sy] lines intersections]
  (vec
   (concat
    (for [[_ x0 y0 x1 y1] lines]
      (let [[px py] (rotate 0.0001 [x0 y0] [sx sy])
            [id _ _ ix iy] (closest-intersection [sx sy px py] all-lines)]
        [id ix iy]))
    (for [[_ x0 y0 x1 y1] lines]
      (let [[px py] (rotate -0.0001 [x0 y0] [sx sy])
            [id _ _ ix iy] (closest-intersection [sx sy px py] all-lines)]
        [id ix iy]))
        (for [[_ x0 y0 x1 y1] lines]
      (let [[px py] (rotate 0.0001 [x1 y1] [sx sy])
            [id _ _ ix iy] (closest-intersection [sx sy px py] all-lines)]
        [id ix iy]))
    (for [[_ x0 y0 x1 y1] lines]
      (let [[px py] (rotate -0.0001 [x1 y1] [sx sy])
            [id _ _ ix iy] (closest-intersection [sx sy px py] all-lines)]
        [id ix iy]))
    (for [[_ _ x0 y0] intersections]
      (let [[px py] (rotate 0.0001 [x0 y0] [sx sy])
            [id _ _ ix iy] (closest-intersection [sx sy px py] all-lines)]
        [id ix iy]))
    (for [[_ _ x0 y0] intersections]
      (let [[px py] (rotate -0.0001 [x0 y0] [sx sy])
            [id _ _ ix iy] (closest-intersection [sx sy px py] all-lines)]
        [id ix iy]))
    (for [[x y] [[0 0] [0 500] [500 0] [500 500]]]
      (let [[id _ _ ix iy] (closest-intersection [sx sy x y] all-lines)]
        [id ix iy])))))

(defn compare-by-angle [[sx sy] [_ ax ay] [_ bx by]]
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

(defn angle-between [[sx sy] [_ ax ay] [_ bx by]]
  (let [angle-a (js/Math.atan2 (- ay sy) (- ax sx))
        angle-b (js/Math.atan2 (- by sy) (- bx sx))
        diff (js/Math.abs (- angle-a angle-b))]
  (min diff (- (* 2 js/Math.PI) diff))))

(/ (angle-between [0 0] [nil 0 -0.1] [nil 0 0.1]) js/Math.PI)

(defn draw []
  (let [sun [(q/mouse-x) (q/mouse-y)]
        impacts (impacts sun lines intersections)
        sorted-impacts (sort-by-angle sun impacts)
        absorbs (make-array num-ids)]
    (println "drawing at" (q/current-frame-rate))
    (println sorted-impacts)
    (q/background 255)
    (q/fill 255 255 0)
    (q/stroke 255 255 0)
    (doseq [i (range (count sorted-impacts))]
      (let [[_ x0 y0] (sorted-impacts i)
            [_ x1 y1] (sorted-impacts (mod (+ i 1) (count sorted-impacts)))]
        (q/triangle (sun 0) (sun 1) x0 y0 x1 y1)))
    (q/stroke 0 0 0)
;;     (doseq [[id ix iy] sorted-impacts]
;;       (when (== id 0)
;;         (q/line (sun 0) (sun 1) ix iy)))
    (doseq [i (range num_ids)]
      (aset absorbs i 0))
    (doseq [i (range (count sorted-impacts))]
      (let [[id0 x0 y0] (sorted-impacts i)
            [id1 x1 y1] (sorted-impacts (mod (+ i 1) (count sorted-impacts)))]
        (when (and (>= id0 0) (== id0 id1))
          (aset absorbs id0 (+ (aget absorbs id0) (angle-between sun [id0 x0 y0] [id1 x1 y1]))))))
    (println (for [i (range (count sorted-impacts))]
      (let [[id0 x0 y0] (sorted-impacts i)
            [id1 x1 y1] (sorted-impacts (mod (+ i 1) (count sorted-impacts)))]
        (when (and (>= id0 0) (== id0 id1))
          (angle-between sun [id0 x0 y0] [id1 x1 y1])))))
    (println absorbs)
    (doseq [[id x0 y0 x1 y1] lines]
      (let [absorb (aget absorbs id)
            brightness (* absorb (/ 10 js/Math.PI))]
        (q/stroke 0 255 0)
        (q/line x0 y0 x1 y1)
        (q/stroke 0)
        (q/fill 0)
        (q/ellipse x0 y0 brightness brightness)
        (q/ellipse x1 y1 brightness brightness)))
    (q/stroke 0 0 0)
    (doseq [[_ x0 y0 x1 y1] edges]
      (q/line x0 y0 x1 y1))))

(defn setup []
  (q/smooth 8)
  (q/frame-rate 30)
  (q/color-mode :rgb))

(q/defsketch sketch
  :setup setup
  :size [501 501]
  :draw draw
  :host "screen")