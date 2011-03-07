(declare create-tank-in-world) ;forward declaration due to circular dep. with tank
(ns ClankAttack.world
  (:use [ClankAttack.tank]))

;the world is a set of references to cells
;cells contain tanks or bullets
;The world is transactional

;The tank has state: its position (and angle)

;dimensions of square world
(def dim 50)
(def max-nr-of-tanks 4)

;flag to set the world running
(def running true)

(defstruct cell :tank) ;may also have :tank and :bullet or :wall

;world is a 2d vector of refs to cells
(def world 
     (apply vector 
            (map (fn [_] 
                   (apply vector (map (fn [_] (ref (struct cell 0))) 
                                      (range dim)))) 
                 (range dim))))

(defn place [[x y]]
  "helper function to look up a cell in the world"
  (-> world (nth x) (nth y)))

(defn place-brick [x y]
  "All in all you're just another brick in the wall..."
  (dosync (alter (place [x y]) assoc :wall 1)))

(defn create-horizontal-wall
  "create a wall in the playing field. From cell (x0 y) to cell (x1 y)"
  [x0 x1 y]
  (map place-brick (range x0 x1) (repeat y)))

(defn create-vertical-wall
  "create a wall in the playing field. From cell (x y0) to cell (x y1)"
  [y0 y1 x]
  (map place-brick (repeat x) (range y0 y1)))

(defn create-random-coordinate
  "create a random coordinate"
  []
  [(rand-int dim) (rand-int dim)])

(defn create-random-coordinates
  "create n unique random coordinates"
  [n]
  (take n (distinct (repeatedly create-random-coordinate))))

;creates tanks in a 80x80 grid now
(defn setup
  "places initial tanks, returns seq of tank agents"
  []
  (sync nil
        (doall
         (map #(create-tank-in-world % (rand-int 8))
              (create-random-coordinates max-nr-of-tanks)))))


