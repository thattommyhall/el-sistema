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

(defn ray-intersection [ray-from ray-to line]
  (let [p0_x (aget ray-from 0)
        p0_y (aget ray-from 1)
        p1_x (aget ray-to 0)
        p1_y (aget ray-to 1)
        p2_x (aget line 1)
        p2_y (aget line 2)
        p3_x (aget line 3)
        p3_y (aget line 4)
        s1_x (- p1_x p0_x)
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
        (array s t ix iy)))))

(defn closest-intersection [ray-from ray-to lines]
  (array-reduce lines
                (fn [closest line]
                  (if-let [impact (ray-intersection ray-from ray-to line)]
                    (if (< (aget impact 1) (aget closest 2))
                      (array (aget line 0) (aget impact 0) (aget impact 1) (aget impact 2) (aget impact 3))
                      closest)
                    closest))
                (array -1 0 js/Number.POSITIVE_INFINITY (aget ray-to 0) (aget ray-to 1))))

(defn rotate [angle [sx sy] [x y]]
  (let [a (q/cos angle)
        b (- (q/sin angle))
        c (- b)
        d a
        vx (- x sx)
        vy (- y sy)
        nx (+ (* a vx) (* b vy))
        ny (+ (* c vx) (* d vy))]
    (array (+ sx nx) (+ sy ny))))

(defn compare-by-angle [centre a b]
  (let [sx (aget centre 0)
        sy (aget centre 1)
        ax (aget a 1)
        ay (aget a 2)
        bx (aget b 1)
        by (aget b 2)]
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
                    (> d1 d2)))))))

(defn sort-by-angle [centre points]
  (vec
   (sort-by identity (fn [px py] (compare-by-angle centre px py)) points)))

(defn angle-between [[sx sy] [_ ax ay] [_ bx by]]
  (let [angle-a (js/Math.atan2 (- ay sy) (- ax sx))
        angle-b (js/Math.atan2 (- by sy) (- bx sx))
        diff (js/Math.abs (- angle-a angle-b))]
  (min diff (- (* 2 js/Math.PI) diff))))

(defn calculate-sunlight [sun trees width height]
  (let [sun (into-array sun)
        lines (into-array (concat
                           (for [[tree id] (map vector trees (range))
                                 [[x0 y0] [x1 y1]] tree]
                             (array id x0 y0 x1 y1))
                           [(array -1 0 0 width 0)
                            (array -1 width 0 width height)
                            (array -1 width height 0 height)
                            (array -1 0 height 0 0)]))
        intersections (array-reduce lines
                                    (fn [acc line0]
                                      (array-reduce lines
                                       (fn [acc line1]
                                         (when-let [intersection (line-intersection line0 line1)]
                                           (.push acc intersection))
                                         acc)
                                       acc))
                                    (array))
        points (vec
                (concat
                 (for [[_ _ x y] intersections]
                   (array x y))
                 (for [[_ x y _ _] lines]
                   (array x y))
                 (for [[_ _ _ x y] lines]
                   (array x y))))
        targets (vec
                 (concat
                  (for [point points]
                    (rotate 0.0001 sun point))
                  (for [point points]
                    (rotate -0.0001 sun point))))
        impacts (into-array
                 (for [target targets]
                   (let [[id _ _ x y] (closest-intersection sun target lines)]
                     (array id x y))))
        absorbs (make-array (count trees))]
    (println (aget impacts 0))
    (sort-by-angle sun impacts)
    (println (aget impacts 0))
    (doseq [id (range (count trees))]
      (aset absorbs id 0))
    (doseq [i (range (alength impacts))]
      (let [[id0 x0 y0] (aget impacts i)
            [id1 x1 y1] (aget impacts (mod (+ i 1) (alength impacts)))]
        (when (and (>= id0 0) (== id0 id1))
          (aset absorbs id0 (+ (aget absorbs id0) (angle-between sun [id0 x0 y0] [id1 x1 y1]))))))
    {:absorbs absorbs
     :impacts impacts}))

;; Rest of file is just for debugging

(enable-console-print!)

(def trees
  (vec
   (for [_ (range 100)]
    [[[(rand-int 500) (rand-int 500)] [(rand-int 500) (rand-int 500)]]])))

(defn draw [sun trees width height]
  (let [{absorbs :absorbs impacts :impacts} (calculate-sunlight sun trees width height)]
    (println "drawing at" (q/current-frame-rate))
    (q/background 255)
    (q/fill 255 255 0)
    (q/stroke 255 255 0)
    (doseq [i (range (alength impacts))]
      (let [[_ x0 y0] (aget impacts i)
            [_ x1 y1] (aget impacts (mod (+ i 1) (alength impacts)))]
        (q/triangle (sun 0) (sun 1) x0 y0 x1 y1)))
    (q/fill 0)
    (q/stroke 0)
    (q/ellipse (sun 0) (sun 1) 10 10)
    (doseq [[tree id] (map vector trees (range))
            [[x0 y0] [x1 y1]] tree]
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

;; (q/defsketch sketch
;;   :setup setup
;;   :size [501 501]
;;   :draw #(draw [(q/mouse-x) (q/mouse-y)] trees 500 500)
;;   :host "screen")