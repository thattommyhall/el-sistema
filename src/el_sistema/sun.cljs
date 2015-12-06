(ns el-sistema.sun
  (:require [quil.core :as q :include-macros true]
            [goog.string]
            [goog.string.format])
  )

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
      (when (and (> s 0) (< s 1) (> t 0) (< t 1))
        [s t ix iy]))))

(defn ray-intersection [[p0_x p0_y] [p1_x p1_y] [_ p2_x p2_y p3_x p3_y]]
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
      (when (and (> s 0) (< s 1) (> t 0))
        [s t ix iy]))))

(defn closest-intersection [ray-from ray-to lines]
  (reduce
   (fn [[id-min s-min t-min ix-min iy-min] line]
     (if-let [[s t ix iy] (ray-intersection ray-from ray-to line)]
       (if (< t t-min)
         [(line 0) s t ix iy]
         [id-min s-min t-min ix-min iy-min])
       [id-min s-min t-min ix-min iy-min]))
   [-1 0 js/Number.POSITIVE_INFINITY (ray-to 0) (ray-to 1)]
   lines))

(defn rotate [angle [sx sy] [x y]]
  (let [a (q/cos angle)
        b (- (q/sin angle))
        c (- b)
        d a
        vx (- x sx)
        vy (- y sy)
        nx (+ (* a vx) (* b vy))
        ny (+ (* c vx) (* d vy))]
    [(+ sx nx) (+ sy ny)]))

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

(defn calculate-sunlight [sun trees width height]
  (let [[sx sy] sun
        lines (vec (concat
                    (for [[tree id] (map vector trees (range))
                         [x0 y0 x1 y1] tree]
                      [id x0 y0 x1 y1])
                    [[-1 0 0 width 0]
                     [-1 width 0 width height]
                     [-1 width height 0 height]
                     [-1 0 height 0 0]]))
        intersections (vec
                       (filter identity
                               (for [line0 lines
                                     line1 lines]
                                 (line-intersection line0 line1))))
        points (vec
                (concat
                 (for [[_ _ x y] intersections]
                   [x y])
                 (for [[_ x y _ _] lines]
                   [x y])
                 (for [[_ _ _ x y] lines]
                   [x y])))
        targets (vec
                 (concat
                  (for [point points]
                    (rotate 0.0001 sun point))
                  (for [point points]
                    (rotate -0.0001 sun point))))
        impacts (for [target targets]
                  (let [[id _ _ x y] (closest-intersection sun target lines)]
                    [id x y]))
        sorted-impacts (sort-by-angle sun impacts)
        absorbs (make-array (count trees))]
    (doseq [id (range (count trees))]
      (aset absorbs id 0))
    (doseq [i (range (count sorted-impacts))]
      (let [[id0 x0 y0] (sorted-impacts i)
            [id1 x1 y1] (sorted-impacts (mod (+ i 1) (count sorted-impacts)))]
        (when (and (>= id0 0) (== id0 id1))
          (aset absorbs id0 (+ (aget absorbs id0) (angle-between sun [id0 x0 y0] [id1 x1 y1]))))))
    {:absorbs absorbs
     :impacts sorted-impacts}))

;; Rest of file is just for debugging

(enable-console-print!)

(def trees
  (vec
   (for [_ (range 10)]
    [[(rand-int 500) (rand-int 500) (rand-int 500) (rand-int 500)]])))

(defn draw []
  (let [sun [(q/mouse-x) (q/mouse-y)]
        {absorbs :absorbs impacts :impacts} (calculate-sunlight sun trees 500 500)]
    (println "drawing at" (q/current-frame-rate))
    (q/background 255)
    (q/fill 255 255 0)
    (q/stroke 255 255 0)
    (doseq [i (range (count impacts))]
      (let [[_ x0 y0] (impacts i)
            [_ x1 y1] (impacts (mod (+ i 1) (count impacts)))]
        (q/triangle (sun 0) (sun 1) x0 y0 x1 y1)))
    (q/fill 0)
    (q/stroke 0)
    (q/ellipse (sun 0) (sun 1) 10 10)
    (doseq [[tree id] (map vector trees (range))
            [x0 y0 x1 y1] tree]
      (let [absorb (aget absorbs id)
            brightness (* absorb (/ 10 js/Math.PI))]
        (q/stroke 0 255 0)
        (q/line x0 y0 x1 y1)
        (q/stroke 255 0 0)
        (q/fill 255 0 0)
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