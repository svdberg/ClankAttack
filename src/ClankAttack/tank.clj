(ns ClankAttack.tank
  (:import (java.awt Color Graphics Dimension)))

(def *tank-radius* 10)

(defrecord Tank [x y id angle dir])

(defn create-tank
  "Randomly create tank"
  [fw fh]
  (let [ angle (rand-int 360)
         dir (/ angle 45)]
    (Tank. (rand-int fw) 
           (rand-int fh)
           (if (zero? (rand-int 2)) :friend :foo)
           angle 
           dir)))

(defn create-tank-without-loc
  "create a tank without place, with a direction.
  dir is a number from 0 to 7"
  [dir]
  (let [angle (* dir 45)]
    (Tank. 0 0
           (if (zero? (rand-int 2)) :friend :foo)
           angle
           dir)))

(defn create-tanks
  "Randomly place n non-overlapping tanks"
  [n fw fh]
  (repeatedly n #(create-tank fw fh)))

(defn get-tank-color [tank]
  (if (= (:id tank) :friend) (Color/green) (Color/blue)))

(defn grad-to-rad [grad]
  (* (/ grad 180) Math/PI))

(defn render-body [g tank]
  (let [r *tank-radius*
        d (* r 2)
        x (- (:x tank) r)
        y (- (:y tank) r)]
    (.fillOval g x y d d)))
  
(defn render-barrel [g tank]
  (let [x1 (:x tank)
        y1 (:y tank)
        alfa (grad-to-rad (:angle tank))
        x2 (+ x1 (* (Math/cos alfa) 20))
        y2 (+ y1 (* (Math/sin alfa) 20))]
    (.drawLine g x1 y1 x2 y2)))

(defn render-tank
  "Render a single tank"
  [g tank]
  (.setColor g (get-tank-color tank))
  (render-body g tank)
  (render-barrel g tank))

(defn render-tanks
  "Render all tanks"
  [g tanks]
  (dorun (map #(render-tank g %) tanks)))

