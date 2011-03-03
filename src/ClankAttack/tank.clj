(ns ClankAttack.tank
  (:use [ClankAttack.world])
  (:import (java.awt Color Graphics Dimension)))

;should be scale
(def *tank-radius* (/ 5 2))

;timing constants
(def tank-sleep-ms 80)
(def bullet-sleep-ms 40)

(defrecord Tank [id angle dir shot])
(defrecord Bullet [dir hit])

(defn create-bullet
  [dir]
  (Bullet. 
    dir false))

(defn create-tank-without-loc
  "create a tank without place, with a direction.
  dir is a number from 0 to 7"
  [dir]
  (let [angle (* dir 45)
        shot false]
    (Tank. 
           (if (zero? (rand-int 2)) :friend :foo)
           angle
           dir
           false)))

(defn create-tank-in-world
  "create an tank at the location, returning a tank agent on the location"
  [loc dir]
    (sync nil
      (let [p (place loc)
            t (create-tank-without-loc dir)]
        (alter p assoc :tank t)
        (agent loc))))

(defn create-bullet-in-world
  "create a bullet at a specific location"
  [loc dir]
  (sync nil
        (let [p (place loc)
              t (create-bullet dir)]
          (alter p assoc :bullet t)
          (agent loc))))

(defn get-tank-color [tank]
  (if (= (:id tank) :friend) (Color/green) (Color/blue)))

(defn grad-to-rad [grad]
  (* (/ grad 180) Math/PI))

(defn render-body [g x y tank]
  (let [r *tank-radius*
        d (* r 2)
        x1 (- x r)
        y1 (- y r)]
    (.fillOval g x1 y1 d d)))
  
(defn render-barrel [g x y tank]
  (let [x1 x 
        y1 y 
        alfa (grad-to-rad (:angle tank))
        x2 (+ x1 (* (Math/sin alfa) 5)) ;what about numeric errors?
        y2 (+ y1 (* (Math/cos alfa) 5))]
    (.drawLine g x1 y1 x2 y2)))

(defn render-tank
  "Render a single tank"
  [g  x y tank]
  (.setColor g (get-tank-color tank))
  (render-body g x y tank)
  (render-barrel g x y tank))

;dirs are 0-7, starting at north and going clockwise
;these are the deltas in order to move one step in given dir
(def dir-delta {0 [0 1]
                1 [1 1]
                2 [1 0]
                3 [1 -1]
                4 [0 -1]
                5 [-1 -1]
                6 [-1 0]
                7 [-1 1]})
(defn bound 
  "returns n wrapped into range 0-b"
  [b n]
    (let [n (rem n b)]
      (if (neg? n) 
        (+ n b) 
        n)))

(defn delta-loc 
"returns the location one step in the given dir. Note the world is a torus"
[[x y] dir]
  (let [[dx dy] (dir-delta (bound 8 dir))]
    [(bound dim (+ x dx)) (bound dim (+ y dy))]))

(defn turn 
  "turns the tank at the location by the given amount"
  [loc amt]
    (dosync
     (let [p (place loc)
           tank (:tank @p)
           new-dir (bound 8 (+ (:dir tank) amt))
           new-angle (* new-dir 45)]
       (alter p assoc :tank (assoc tank :angle new-angle :dir new-dir))))
    loc)

(defn move 
  "moves the tank in the direction it is heading. Must be called in a
  transaction that has verified the way is clear"
  [loc]
     (let [oldp (place loc)
           tank (:tank @oldp)
           newloc (delta-loc loc (:dir tank))
           p (place newloc)]
         ;move the tank
       (alter p assoc :tank tank)
       (alter oldp assoc :tank 0)
  newloc))

(defn move-bullet
  "moves the bullet in the direction Must be called in a
  transaction that has verified the way is clear"
  [loc]
  (let [oldp (place loc)
        bullet (:bullet @oldp)
        ;newloc (delta-loc (delta-loc loc (:dir bullet)) (:dir bullet))
        newloc (delta-loc loc (:dir bullet)) 
        p (place newloc)]
    (alter p assoc :bullet bullet)
    (alter oldp dissoc :bullet)
  newloc))

(defn shoot-tank
  "updates the state of a tank to shot"
  [loc]
  (let [p (place loc)
        tank (:tank @p)
        bullet (:bullet @p)
        changed-tank (assoc tank :shot true)
        changed-bullet (assoc bullet :hit true)]
      (alter p assoc :tank changed-tank)
      (alter p assoc :bullet changed-bullet)
      loc))

(defn hit-wall
  "updates the state of a bullet, when a wall is hit"
  [loc]
  (let [p (place loc)
        bullet (:bullet @p)
        changed-bullet (assoc bullet :hit true)]
      (alter p assoc :bullet changed-bullet)
      loc))

(defn bullet-behave
  "bullet behaviour agent, flies a bullet
   three options:
   1. there is a tank ahead => tank gets shot, bullet hits
   2. there is a tank ahead ahead => tank gets shot, bullet hits
   3. there is no tank ahead, move forward"
  [loc]
  (let [p (place loc)
        bullet (:bullet @p)
        hit? (:hit bullet)
        ahead (place (delta-loc loc (:dir bullet)))]
  (. Thread (sleep bullet-sleep-ms))
  (dosync
      (when (and running (not hit?))
        (send-off *agent* #'bullet-behave))
      (if
        (not= (:tank @p) 0)
          ;there is a tank ahead, it gets shot
          (shoot-tank loc) 
        ;else
          (cond
            (= (:wall @ahead) 1)
              ;hit a wall, stop!  
              (hit-wall loc)
            :else
              (move-bullet loc))))))

(defn behave
  "the basic behaviour of a tank"
  [loc]
  (let [ p (place loc)
         tank (:tank @p)
         hit? (:shot tank)
         new-loc (delta-loc (delta-loc loc (:dir tank)) (:dir tank))
         ahead (place new-loc)
         rnd-int (rand-int 10)]
    (. Thread (sleep tank-sleep-ms))
    (dosync
      (when (and running (not hit?))
        (send-off *agent* #'behave))
      (when (and (> rnd-int 8) (not hit?))
        (send-off (create-bullet-in-world new-loc (:dir tank)) bullet-behave)) ; is this correct, maybe loc?
      (cond
        (= (:wall @ahead) 1)
          (-> loc (turn 4))
        (= (:tank @ahead) 0)
          (move loc)
        :else
          loc))))

