(ns ClankAttack.world
  (:use [ClankAttack.tank]))

;the world is a set of references to cells
;cells contain tanks or bullets
;The world is transactional
;
;The tank has state: its position (and angle)

;dimensions of square world
(def dim 10)
(def max-nr-of-tanks 2)

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
            t (create-tank-without-loc dir)]
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

;dirs are 0-7, starting at north and going clockwise
;these are the deltas in order to move one step in given dir
(def dir-delta {0 [0 -1]
                1 [1 -1]
                2 [1 0]
                3 [1 1]
                4 [0 1]
                5 [-1 1]
                6 [-1 0]
                7 [-1 -1]})
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
