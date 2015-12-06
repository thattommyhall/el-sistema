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

(def num-rays 1000)
(def max-absorbs 5)
(def absorb-rate 2)

(defn find-impacts [centre lines targets]
  (let [sx (aget centre 0)
        sy (aget centre 1)
        max-angle js/Math.PI
        inc-angle (/ max-angle num-rays)
        impacts (into-array (for [_ (range max-absorbs)] (array)))
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
            (let [aim (array (+ sx (js/Math.cos angle)) (+ sy (js/Math.sin angle)))
                  impacts-here (array)]
              (areduce lines-in-sight ix _ nil
                       (let [line (aget lines (aget lines-in-sight ix))]
                         (when-let [impact (ray-intersection centre aim line)]
                           (.push impacts-here impact))))
              (goog.array/sort impacts-here (fn [a b] (goog.array/defaultCompare (aget a 2) (aget b 2))))
              (when (> (alength impacts-here) 0) ; TODO why do rays ever miss the edges?
                (dotimes [ix max-absorbs]
                  (.push (aget impacts ix) (aget impacts-here (min ix (- (alength impacts-here) 1))))))
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
    (dotimes [a max-absorbs]
      (let [impacts (aget impacts a)
            absorb (/ 1 (js/Math.pow absorb-rate a))]
        (areduce impacts i _ nil
                 (let [impact0 (aget impacts i)
                       impact1 (aget impacts (mod (+ i 1) (alength impacts)))
                       id0 (aget impact0 0)
                       id1 (aget impact1 0)]
                   (when (and (>= id0 0) (== id0 id1))
                     (aset absorbs id0 (+ (aget absorbs id0) absorb)))))))
    {:absorbs absorbs
     :impacts impacts
     :lines lines
     :points points}))

;; Rest of file is just for debugging

(enable-console-print!)

(def trees
  (vec
   (for [_ (range 100)]
     (let [x0 (+ 100 (rand-int 300))
           y0 (+ 100 (rand-int 300))
           x1 (+ x0 -100 (rand-int 200))
           y1 (+ y0 -100 (rand-int 200))]
       [[[x0 y0] [x1 y1]]]))))

(defn draw [sun trees width height]
  (let [{absorbs :absorbs impacts :impacts lines :lines points :points} (calculate-sunlight sun trees width height)]
    (println "drawing at" (q/current-frame-rate))
    (q/background 0)
    (let [[sx sy] sun]
      (q/with-graphics (q/state :light)
        (q/background 0)
        (dotimes [a max-absorbs]
          (let [a (- max-absorbs 1 a)
                impacts (aget impacts a)]
            (q/fill (/ 255 (js/Math.pow absorb-rate a)))
            (q/stroke (/ 255 (js/Math.pow absorb-rate a)))
            (if (== a 0)
              (do
                (q/begin-shape)
                (areduce impacts i _ nil
                         (let [impact (aget impacts i)]
                           (q/vertex (aget impact 3) (aget impact 4))))
                (q/end-shape))
              (do
                (q/begin-shape)
                (areduce impacts i _ nil
                         (let [impact (aget impacts i)]
                           (q/vertex (aget impact 3) (aget impact 4))))
                (q/end-shape)))))))
    (q/stroke 0 255 0)
    (areduce lines i _ nil
             (let [line (aget lines i)]
               (q/line (aget line 1) (aget line 2) (aget line 3) (aget line 4))))
    (q/stroke 255 0 0)
    (q/fill 255 0 0)
    (areduce lines i _ nil
             (let [line (aget lines i)
                   id (aget line 0)
                   absorb (aget absorbs id)
                   brightness (/ absorb 100)]
               (q/ellipse (aget line 1) (aget line 2) brightness brightness)
               (q/ellipse (aget line 3) (aget line 4) brightness brightness)))
    (q/tint 200 200 100 220)
    (q/image (q/state :light) 0 0)
    (q/fill 255)
    (q/stroke 255)
    (q/ellipse (sun 0) (sun 1) 20 20)
    absorbs))

(defn setup []
  (q/smooth 0)
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (q/set-state! :light (q/create-graphics 500 500)))

;; (q/defsketch sketch
;;   :setup setup
;;   :size [501 501]
;;   :draw #(draw [(q/mouse-x) (q/mouse-y)] trees 500 500)
;;   :host "screen")
