(ns ClankAttack.world
  (:use [ClankAttack.tank]))

;the world is a set of references to cells
;cells contain tanks or bullets
;The world is transactional
;
;The tank has state: its position (and angle)

;dimensions of square world
(def dim 80)
(def max-nr-of-tanks 10)

;(defrecord Cell [tank])
(defstruct cell :tank) ;may also have :tank and :bullet

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

;name conflict with the tank creation function itself?
(defn create-tank-in-world
  "create an tank at the location, returning a tank agent on the location"
  [loc dir]
    (sync nil
      (let [p (place loc)
            t (t/create-tank-without-loc dir)]
        (alter p assoc :tank t)
        (agent loc))))

;creates 100 tanks in a 80x80 grid now
(defn setup 
  "places initial tanks, returns seq of tank agents"
  []
  (sync nil
    (doall
     (for [x (repeatedly max-nr-of-tanks #(rand-int dim))
           y (repeatedly max-nr-of-tanks #(rand-int dim))]
       (do
         (create-tank-in-world [x y] (rand-int 8)))))))
