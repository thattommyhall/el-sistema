(ns el-sistema.sun
  (:require [quil.core :as q :include-macros true]
            [goog.string]
            [goog.string.format]
            [goog.array])
  )

(defn line-intersection [line0 line1]
  (let [p0_x (aget line0 1)
        p0_y (aget line0 2)
        p1_x (aget line0 3)
        p1_y (aget line0 4)
        p2_x (aget line1 1)
        p2_y (aget line1 2)
        p3_x (aget line1 3)
        p3_y (aget line1 4)
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
      (when (and (> s 0) (< s 1) (> t 0) (< t 1))
        (array s t ix iy)))))

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
  (let [closest (array -1 0 js/Number.POSITIVE_INFINITY (aget ray-to 0) (aget ray-to 1))]
    (areduce lines ix _ nil
             (let [line (aget lines ix)]
               (when-let [impact (ray-intersection ray-from ray-to line)]
                 (when (< (aget impact 1) (aget closest 2))
                   (aset closest 0 (aget line 0))
                   (aset closest 1 (aget impact 0))
                   (aset closest 2 (aget impact 1))
                   (aset closest 3 (aget impact 2))
                   (aset closest 4 (aget impact 3))))))
    closest))

(defn rotate [angle centre point]
  (let [sx (aget centre 0)
        sy (aget centre 1)
        x (aget point 0)
        y (aget point 1)
        a (q/cos angle)
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
     (and (>= (- ax sx) 0) (< (- bx sx) 0)) 1
     (and (< (- ax sx) 0) (>= (- bx sx) 0)) -1
     (and (== (- ax sx) 0) (== (- bx sx) 0)) (if (or (>= (- ay sy) 0) (>= (- by sy) 0))
                                               (if (> ay by) 1 -1)
                                               (if (> by ay) 1 -1))
     true (let [det (-
                     (* (- ax sx) (- by sy))
                     (* (- bx sx) (- ay sy)))]
            (cond
             (< det 0) 1
             (> det 0) -1
             true (let [d1 (+
                            (* (- ax sx) (- ax sx))
                            (* (- ay sy) (- ay sy)))
                        d2 (+
                            (* (- bx sx) (- bx sx))
                            (* (- by sy) (- by sy)))]
                    (if (> d1 d2) 1 -1)))))))

(defn sort-by-angle [centre points]
  (goog.array/stableSort points (fn [px py] (compare-by-angle centre px py))))

(defn angle-between [centre a b]
  (let [sx (aget centre 0)
        sy (aget centre 1)
        ax (aget a 1)
        ay (aget a 2)
        bx (aget b 1)
        by (aget b 2)
        angle-a (js/Math.atan2 (- ay sy) (- ax sx))
        angle-b (js/Math.atan2 (- by sy) (- bx sx))
        diff (js/Math.abs (- angle-a angle-b))]
  (min diff (- (* 2 js/Math.PI) diff))))

(defn calculate-sunlight [sun trees width height]
  (let [sun (into-array sun)
        lines (array)
        _ (doseq [[tree id] (map vector trees (range))
                  [[x0 y0] [x1 y1]] tree]
            (.push lines (array id x0 y0 x1 y1)))
        _ (.push lines (array -1 0 0 width 0))
        _ (.push lines (array -1 width 0 width height))
        _ (.push lines  (array -1 width height 0 height))
        _ (.push lines (array -1 0 height 0 0))
        intersections (array)
        _ (areduce lines ix0 _ nil
                   (areduce lines ix1 _ nil
                            (when-let [intersection (line-intersection (aget lines ix0) (aget lines ix1))]
                              (.push intersections intersection))))
        points (array)
        _ (areduce intersections ix _ nil
                   (let [intersection (aget intersections ix)]
                     (.push points (array (aget intersection 2) (aget intersection 3)))))
        _ (areduce lines ix _ nil
                   (let [line (aget lines ix)]
                     (.push points (array (aget line 1) (aget line 2)))))
        _ (areduce lines ix _ nil
                   (let [line (aget lines ix)]
                     (.push points (array (aget line 3) (aget line 4)))))
        targets (array)
        _ (areduce points ix _ nil
                   (.push targets (rotate 0.0001 sun (aget points ix))))
        _ (areduce points ix _ nil
                   (.push targets (rotate -0.0001 sun (aget points ix))))
        impacts (array)
        _ (areduce targets ix _ nil
                   (let [intersection (closest-intersection sun (aget targets ix) lines)]
                     (.push impacts (array (aget intersection 0) (aget intersection 3) (aget intersection 4)))))
        absorbs (make-array (count trees))]
    (sort-by-angle sun impacts)
    (doseq [id (range (count trees))]
      (aset absorbs id 0))
    (doseq [i (range (alength impacts))]
      (let [[id0 x0 y0] (aget impacts i)
            [id1 x1 y1] (aget impacts (mod (+ i 1) (alength impacts)))]
        (when (and (>= id0 0) (== id0 id1))
          (aset absorbs id0 (+ (aget absorbs id0) (angle-between sun (array id0 x0 y0) (array id1 x1 y1)))))))
    {:absorbs absorbs
     :impacts impacts}))

;; Rest of file is just for debugging

(enable-console-print!)

(def trees
  (vec
   (for [_ (range 1000)]
     (let [x0 (rand-int 500)
           y0 (rand-int 500)
           x1 (+ x0 (rand-int 25))
           y1 (+ y0 (rand-int 25))]
       [[[x0 y0] [x1 y1]]]))))

(defn draw [sun trees width height]
  (let [{absorbs :absorbs impacts :impacts} (calculate-sunlight sun trees width height)]
    ;; (println "drawing at" (q/current-frame-rate))
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
      (q/line x0 y0 x1 y1))
    absorbs))

(defn setup []
  (q/smooth 8)
  (q/frame-rate 30)
  (q/color-mode :rgb))

;; (q/defsketch sketch
;;   :setup setup
;;   :size [501 501]
;;   :draw #(draw [(q/mouse-x) (q/mouse-y)] trees 500 500)
;;   :host "screen")
