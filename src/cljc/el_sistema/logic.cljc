(ns el-sistema.logic
  #?(:cljs (:require [cljs.reader :refer [read-string]])))

; Plant


(defrecord Branch [angle length children])

(defrecord Plant [genome branch energy x total-energy])


;; Aux functions
(defn ->rad [degrees]
  (* (/ degrees 360)
     (* 2 Math/PI)))

(defn ->degrees [radians]
  (* radians (/ 180 Math/PI)))

(defn move [[x y angle] units]
  [(+ x (* units (Math/sin angle)))
   (+ y (* units (Math/cos angle)))
   angle])


(defn rotate [[x y angle] rads]
  [x y (+ angle rads)])

;; constants

;;; Length consumed by creating a new branch
(def branching-consumed-length 5)

(defn compute-energy-cost [height]
  ;;(Math/pow 1.1 height)
  ;;(* 0.2 height)
  (* 0.01
     (* height height)))

;; condition and actions

(defrecord Action [consumed-length action-fn type])

(defn make-condition [op target value]
  (fn [branch height]
    (let [target-value (condp = target
                         :length (:length branch)
                         :angle  (:angle branch)
                         :height height)]
      (op target-value value))))


(defn make-and-condition [conditions]
  (fn [branch height]
    (reduce #(and %1 %2) (map #(% branch height) conditions))))

(defn make-or-condition [conditions]
  (fn [branch height]
    (reduce #(or %1 %2) (map #(% branch height) conditions))))

(defn make-grow-action [length]
  (->Action length
            (fn [branch energy energy-per-length-unit]
              (let [final-energy (- energy (* length energy-per-length-unit))]
                [final-energy (assoc branch :length (+ (:length branch) length))]))
            :growth))

(defn make-branch-action [branches]
  (let [branching-consumed-energy (* branching-consumed-length (count branches))]
    (->Action branching-consumed-energy
              (fn [branch energy energy-per-length-unit]
                (let [new-branches (map (fn [angle] (map->Branch {:angle (->rad angle), :length 0 :children []})) branches)]
                  [(- energy (* energy-per-length-unit branching-consumed-energy))
                   (assoc branch :children new-branches)]))
              :branching)))

;;; parsing functions

(defn parse-float [value]
  #?(:cljs (js/parseFloat (str value))
     :clj (Float/parseFloat (str value))))

(defn parse-target [target]
  (condp = target
    'length :length
    'angle :angle
    'height :height
    (throw (#?(:clj Exception. :cljs js/Error.) (str "Invalid target: " target)))))

(defn parse-operator [operator]
  (condp = operator
    '= =
    '> >
    '< <
    '>= >=
    '<= <=
    'not= not=
    (throw (#?(:clj Exception. :cljs js/Error.) (str "Invalid operator: " operator)))))

(defn parse-single-condition
  "(op length|angle float)"
  [[op target value]]
  (make-condition (parse-operator op)
                  (parse-target target)
                  (parse-float value)))

'true
'false  (fn [_branch _height] false)

(defn parse-conditions
  "(and|or conditions|single-conditions)"
  [conditions]
  (if (= conditions 'true)
    (fn [_branch _height] true)
    (if (= conditions 'false)
      (fn [_branch _height] false)
      (if (seq conditions)
        (condp = (first conditions)
          'and (make-and-condition (map parse-conditions (rest conditions)))
          'or  (make-or-condition (map parse-conditions (rest conditions)))
          (parse-single-condition conditions))
        (throw (#?(:clj Exception. :cljs js/Error.) (str "Invalid condition: " (doall conditions))))))))

(defn parse-branch-action [angle]
  (let [angle-str (str angle)]
    (parse-float angle-str)))

(defn parse-action [[action-name & action-body]]
  (condp = action-name
    'grow (make-grow-action (parse-float (first action-body)))
    'branch (make-branch-action (map parse-branch-action action-body))
    (throw (#?(:clj Exception. :cljs js/Error.) (str "Invalid action: " action-name)))))

(defn parse-rule [rule]
  (let [[_ conditions _ action] rule
        parsed-conditions (parse-conditions conditions)
        parsed-action (parse-action action)]
    [parsed-conditions parsed-action]))

(defn parse-rules [rules]
  (map parse-rule rules))

(defn parse-genome [genome]
  (let [[_ & rules] genome]
    (parse-rules rules)))

(defn parse-genome-string [genome-string]
  (->> genome-string (read-string) (parse-genome) doall))

(defn seed [genome] (map->Plant {:genome genome
                                 :branch (map->Branch {:angle (->rad 0), :length 5, :children []})
                                 :energy 0
                                 :total-energy 0}))

(defn compute-final-height [height length angle]
  (+ height (* length (Math/cos (->rad angle)))))

(declare evolve)

(defn evolve-branches [genome branches energy height]
  (loop [remaining-branches branches
         remaining-energy energy
         new-branches []]
    (if (seq remaining-branches)
      (let [[next-branch & rest-branches] remaining-branches
            [new-remaining-energy new-next-branch] (evolve genome next-branch remaining-energy height)]
        (recur rest-branches
               new-remaining-energy
               (cons new-next-branch new-branches)))
      [remaining-energy (concat (reverse new-branches) remaining-branches)])))

(defn evolve [genome {:keys [angle length children] :as branch} energy height]
  (if (or (<= energy 0) (< height 0))
    [energy branch]
    (let [energy-per-length-unit (compute-energy-cost height)
          can-grow (/ energy energy-per-length-unit)
          has-branches (not (empty? children))
          rules (if has-branches (filter #(not= :branching (:type (last %))) genome) genome)
          rules (filter #(<= (:consumed-length (last %)) can-grow) rules)
          rules (filter #((first %) branch height) rules)]
      (if (empty? rules)
        (let [final-height (compute-final-height height length angle)
              [final-energy new-children] (evolve-branches genome children energy final-height)]
          [final-energy (assoc branch :children new-children)])
        (let [action-fn (-> rules first last :action-fn)
              [remaining-energy {:keys [angle length children] :as branch}] (action-fn branch energy energy-per-length-unit)
              final-height (compute-final-height height length angle)
              [final-energy new-children] (evolve-branches genome children remaining-energy final-height)]
          [final-energy (assoc branch :children new-children)])))))

(defn evolve-plant [{:keys [genome branch energy total-energy] :as plant} increment-energy ]
  (let [available-energy (+ energy increment-energy)
        [remaining-energy new-branch] (evolve genome branch available-energy 0)
        remaining-energy (if (< remaining-energy 0) 0 remaining-energy)]
    (-> plant
        (assoc :branch new-branch)
        (assoc :energy remaining-energy)
        (update :total-energy #(+ % increment-energy)))))
;;;

(defn plant->segs
  ([x plant]
   ;; (println plant)
   (plant->segs (:branch plant)
                [x (:length (:branch plant)) (->rad 0)]
                [[[x 0] [x (:length (:branch plant))]]]))
  ([{:keys [angle children length]} [x y tree-angle :as position] accum]
   (let [[_ _ anglep] (rotate position angle)
         [xp yp _ ] (move [x y anglep] length)
         segment [[x y] [xp yp]]]
     (loop [children children
            accum (conj accum segment)]
       (if (seq children)
         (recur (rest children)
                (plant->segs (first children) [xp yp anglep] accum))
         accum)))))

(defn plant->string
  ([plant]
   (letfn [(branch->string [branch]
             (let [acc (str "F_" (:length branch))
                   children_acc (map #(str "[+" (Math/round (->degrees (:angle %))) (plant->string {:branch %}) "]") (:children branch))]
               (str acc (apply str children_acc))))]
     (branch->string (:branch plant)))))

;;;;

;(def sample-genome (parse-genome (read-string "(genome
;                                                (rule (< length 50) => (grow 10))
;                                                (rule (>= length 50) => (branch -45 +45)))")))
;
;(doseq [plant (take 190 (iterate (partial evolve-plant 20) (seed sample-genome)))]
;  (println "ENERGY " (:energy plant) " -> " (plant->string plant))
;  (println "SEGMENTS " (plant->segs 100 plant)))
