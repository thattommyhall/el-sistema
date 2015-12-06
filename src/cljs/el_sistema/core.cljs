(ns el-sistema.core
  (:require [clojure.browser.repl :as repl]
            [matchbox.core :as m]
            [quil.core :as q :include-macros true]
            [el-sistema.logic :as logic]
            [el-sistema.sun :as sun]))


(enable-console-print!)
(println "TOM TEST ")

;(defonce conn
;  (repl/connect "http://localhost:9000/repl"))


(defn move [[x y angle] units]
  [(+ x (* units (Math/sin angle)))
   (+ y (* units (Math/cos angle)))
   angle])

(defn ->rad [degrees]
  (* (/ degrees 360)
     (* 2 Math/PI)))

(defn rot [[x y angle] degrees]
  [x y (+ angle (->rad degrees))])

(def default-angle 25)
(def default-length 10)

(defmulti process-instruction (fn [instruction _ _] instruction))
(defmethod process-instruction \X [_ position stack] [nil position stack])
(defmethod process-instruction \- [_ position stack] [nil (rot position (- default-angle)) stack])
(defmethod process-instruction \+ [_ position stack] [nil (rot position (+ default-angle)) stack])
(defmethod process-instruction \F [_ [x y angle :as position] stack]
  (let [[xp yp _ :as new-position] (move position default-length)
        segment [[x y] [xp yp]]]
    [segment new-position stack]))
(defmethod process-instruction \[ [_ position stack] [nil position (cons position stack)])
(defmethod process-instruction \] [_ _ [position & stack]] [nil position stack])

(defn tree-segs
  ([phoenotype]
   (tree-segs phoenotype [0 0 (->rad 90)]))
  ([phoenotype pos]
   (tree-segs phoenotype pos [] []))
  ([phoenotype position accum stack]
   (if (seq phoenotype)
     (let [[new-segment new-position new-stack] (process-instruction (first phoenotype) position stack)]
       (recur (rest phoenotype)
              new-position
              (conj accum new-segment)
              new-stack))
     (filter (comp not nil?) accum))))

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

(def ruleset {\X "F-[[X]+X]+F[+FX]-X"
              \F "FF"})

(defn mapchar [ruleset c]
  (get ruleset c c))

(defn apply-rule [ruleset [l [h & t :as r]]]
  (if (empty? r)
    [""
     (apply str l)]
    [(apply str [l (mapchar ruleset h)] )
     (apply str t)]))

(defn sequence-for [ruleset init]
  (distinct (map (fn [[l r]]
                   (apply str l r))
                 (iterate (partial apply-rule ruleset) ["" init]))))

(def tree-sequence (sequence-for ruleset "X"))

(defn segs [n]
  (tree-segs (nth tree-sequence n) [50 500 90]))

(def depth (atom 0))

(println "blah!!")
(def sample-genome (logic/parse-genome-string "(genome
                                                  (rule (< length 50)  => (grow 10))
                                                  (rule (>= length 50) => (branch +10 -10))
                                                  )"))

(defn print-plant-string [plant] (println "PLANT:" (logic/plant->string plant)) plant)
(defn print-plant-energy [plant] (println "ENERGY:" (:energy plant)) plant)
(defn print-plant-segments [plant] (println "SEGMENTS " (logic/plant->segs 100 plant)) plant)

(defn segs [n]
  (println "INVOKING SEGS " n)
  (->> (iterate (partial logic/evolve-plant 100) (logic/seed sample-genome))
       (take n)
       (last) ; last iteration
       (print-plant-string)
       (print-plant-energy)
       (print-plant-segments)
       (logic/plant->segs 100)))

(defn draw []
  ;; (println segs)
  (q/background 100)
  ;; (q/fill 0)
  (q/stroke-float 0)
  (let [nsegs (segs @depth)]
    (println nsegs)
    (println "PRINTING!!! " (count nsegs))
    (doseq [[start stop] nsegs]
      (q/line start stop))
    ;(sun/draw [(q/mouse-x) (q/mouse-y)] [nsegs] 600 400)
    )

  (swap! depth inc))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  (q/frame-rate 10)
  ;; (q/color-mode :rgb)
  )


(q/defsketch hello
  :setup setup
  :draw draw
  :host "tree"
  :size [600 400]
  )

;(println "NOW!!!")
;(println (segs 50))
