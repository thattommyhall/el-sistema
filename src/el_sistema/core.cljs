(ns el-sistema.core
  (:require [clojure.browser.repl :as repl]
            [matchbox.core :as m]
            [quil.core :as q :include-macros true])
  )

;; (defonce conn
;;   (repl/connect "http://localhost:9000/repl"))

(defn draw []
  (q/background 255)
  (q/fill 0)
  (q/ellipse 56 46 55 55))

(q/defsketch hello
  :draw draw
  :host "screen"
  :size [300 300])

(defonce conn
    (repl/connect "http://localhost:9000/repl"))

(enable-console-print!)

(println "Hello world!")

(def root (m/connect "https://el-sistema.firebaseio.com"))

(m/auth-anon root)

(m/listen-children
  root [:users :mike :friends]
  (fn [[event-type data]] (prn event-type data)))

(def mikes-friends (m/get-in root [:users :mike :friends]))
(m/reset! mikes-friends [{:name "Kid A"} {:name "Kid B"}])
(m/conj! mikes-friends {:name "Jean"})

(m/deref
  mikes-friends
  (fn [key value]
    (m/reset-in! root [:users :mike :num-friends]
                 (count value))))

(def R {\X "F-[[X]+X]+F[+FX]-X"
        \F "FF"})

(defn apply-rule [R phenotype]
  (apply str (map R phenotype)))
