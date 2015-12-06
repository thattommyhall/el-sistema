(ns el-sistema.sun
  (:require [quil.core :as q :include-macros true]
            [goog.string]
            [goog.string.format]
            [goog.array])
  )

(defn ray-intersection [ray-from ray-to line]
  (let [p0_x (aget ray-from 0)
        p0_y (aget ray-from 1)
        p1_x (aget ray-to 0)
        p1_y (aget ray-to 1)
        id (aget line 0)
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
        (array id s t ix iy)))))

(defn make-clockwise [centre line]
  (let [sx (aget centre 0)
        sy (aget centre 1)
        ax (aget line 1)
        ay (aget line 2)
        bx (aget line 3)
        by (aget line 4)
        angle-a (js/Math.atan2 (- ay sy) (- ax sx))
        angle-b (js/Math.atan2 (- by sy) (- bx sx))
        diff (- angle-b angle-a)]
    (when (or (and (>= diff 0) (< diff js/Math.PI))
              (< diff (- js/Math.PI)))
      (aset line 1 bx)
      (aset line 2 by)
      (aset line 3 ax)
      (aset line 4 ay))))

(defn array-remove [array value]
  (let [i (.indexOf array value)]
    (when (>= i 0)
      (let [last (.pop array)]
        (when (< i (alength array))
          (aset array i last))))))

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

(defn find-impacts [centre lines targets]
  (let [sx (aget centre 0)
        sy (aget centre 1)
        max-angle js/Math.PI
        inc-angle (/ max-angle 1000)
        impacts (array)
        lines-in-sight (array)]
    (areduce targets ix _ nil
             (let [target (aget targets ix)
                   kind (aget target 0)]
               (if (== kind 1)
                 (.push lines-in-sight (aget target 3))
                 (array-remove lines-in-sight (aget target 3)))))
    (.push targets (array -1 1 0 max-angle)) ;; dummy point at the end so loop doesn't run out of targets
    (loop [angle (- js/Math.PI)
           ix 0]
      (when (< angle max-angle)
        (let [target (aget targets ix)
              kind (aget target 0)
              target-angle (aget target 4)]
          (if (< target-angle angle)
            (do
              (if (== kind 1)
                 (.push lines-in-sight (aget target 3))
                 (array-remove lines-in-sight (aget target 3)))
              (recur angle (+ ix 1)))
            (let [aim (array (+ sx (js/Math.cos angle)) (+ sy (js/Math.sin angle)))]
              (.push impacts
                     (areduce lines-in-sight ix closest (array -1 0 js/Number.POSITIVE_INFINITY sx sy)
                              (let [line (aget lines (aget lines-in-sight ix))]
                                (if-let [impact (ray-intersection centre aim line)]
                                  (if (< (aget impact 2) (aget closest 2))
                                    impact
                                    closest)
                                  closest))))
              (recur (+ angle inc-angle) ix))))))
    impacts))

(defn calculate-sunlight [sun trees width height]
  (let [sun (into-array sun)
        [sx sy] sun
        id (array 0)
        lines (array)
        _ (reduce
           (fn [_ tree]
             (reduce
              (fn [_ [[x0 y0] [x1 y1]]]
                (.push lines (array (aget id 0) x0 y0 x1 y1)))
              nil
              tree)
             (aset id 0 (+ (aget id 0) 1)))
           nil
           trees)
        _ (.push lines (array -1 0 0 width 0))
        _ (.push lines (array -1 width 0 width height))
        _ (.push lines  (array -1 width height 0 height))
        _ (.push lines (array -1 0 height 0 0))
        _ (areduce lines ix _ nil
                   (make-clockwise sun (aget lines ix)))
        points (array)
        _ (areduce lines ix _ nil
                   (let [line (aget lines ix)]
                     (.push points (array -1 (aget line 1) (aget line 2) ix))))
        _ (areduce lines ix _ nil
                   (let [line (aget lines ix)]
                     (.push points (array 1 (aget line 3) (aget line 4) ix))))
        _ (areduce points ix _ nil
                   (let [point (aget points ix)
                         angle (js/Math.atan2 (- (aget point 2) sy) (- (aget point 1) sx))]
                     (.push point angle)))
        _ (goog.array/stableSort points (fn [p0 p1] (goog.array/defaultCompare (aget p0 4) (aget p1 4))))
        impacts (find-impacts sun lines points)
        absorbs (make-array (count trees))]
    (doseq [id (range (count trees))]
      (aset absorbs id 0))
    (areduce impacts i _ nil
             (let [impact0 (aget impacts i)
                   id0 (aget impact0 0)
                   x0 (aget impact0 3)
                   y0 (aget impact0 4)
                   impact1 (aget impacts (mod (+ i 1) (alength impacts)))
                   id1 (aget impact1 0)
                   x1 (aget impact1 3)
                   y1 (aget impact1 4)]
               (when (and (>= id0 0) (== id0 id1))
                 (aset absorbs id0 (+ (aget absorbs id0) (angle-between sun (array id0 x0 y0) (array id1 x1 y1)))))))
    {:absorbs absorbs
     :impacts impacts
     :lines lines
     :points points}))

;; Rest of file is just for debugging

(enable-console-print!)

(def trees
  (vec
   (for [_ (range 1000)]
     (let [x0 (+ 100 (rand-int 300))
           y0 (+ 100 (rand-int 300))
           x1 (+ x0 -10 (rand-int 20))
           y1 (+ y0 -10 (rand-int 20))]
       [[[x0 y0] [x1 y1]]]))))

(defn draw [sun trees width height]
  (let [{absorbs :absorbs impacts :impacts points :points} (calculate-sunlight sun trees width height)]
    (println "drawing at" (q/current-frame-rate))
    (q/background 255)
    (q/fill 255 255 0)
    (q/stroke 255 255 0)
;;     (doseq [i (range (alength impacts))]
;;       (let [[_ _ _ x0 y0] (aget impacts i)
;;             [_ _ _ x1 y1] (aget impacts (mod (+ i 1) (alength impacts)))]
;;         (q/triangle (sun 0) (sun 1) x0 y0 x1 y1)))
    (q/stroke 0)
    (q/fill 0)
    (doseq [i (range (alength impacts))]
      (let [[_ _ _ x y] (aget impacts i)]
        (q/line (sun 0) (sun 1) x y)))
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
