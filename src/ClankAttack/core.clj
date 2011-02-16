(ns ClankAttack.core
  (:use [ClankAttack.tank])
  (:use [ClankAttack.world])
  (:import (javax.swing JFrame JPanel))
  (:import (java.awt Color Graphics Dimension))
  (:import (java.awt.image BufferedImage)))

; bunch of constants
; equals a 10x10 grid world
(def *field-width* 500)
(def *field-height* 500)
  
;pixels per world cell
(def scale 5)

(def running true)
(def animation-sleep-ms 100)

(defn render-background [g img]
  (doto g
    (.setColor (Color. 255 230 255))
    (.fillRect 0 0 (.getWidth img) (.getHeight img))))

;(defn render [g tanks]
;  (let [img (new BufferedImage *field-width* *field-height*
;                 (. BufferedImage TYPE_INT_ARGB))
;        bg (.getGraphics img)]
;    (render-background bg img)
;    (render-tanks bg tanks)
;    (.drawImage g img 0 0 nil)
;    (.dispose bg)))

;(defn create-panel [tanks]
;  (doto (proxy [JPanel] []
;          (paint [g] (render g tanks)))
;    (.setPreferredSize (new Dimension *field-width* *field-height*))))

;(defn draw-battle-field [tanks]
;  (doto (new JFrame "Clank Attack")
;    (.add (create-panel tanks))
;    .pack
;    .show))

;new world drawing
(defn render-a-tank [tank #^Graphics g x y]
  (let [t (assoc tank :x x :y y)]
    (render-tank g t)))
;  (let [r *tank-radius*
;        d (* r 2)
;        [hx hy tx ty] ({0 [2 0 2 4] 
;                        1 [4 0 0 4] 
;                        2 [4 2 0 2] 
;                        3 [4 4 0 0] 
;                        4 [2 4 2 0] 
;                        5 [0 4 4 0] 
;                        6 [0 2 4 2] 
;                        7 [0 0 4 4]}
;                       (:dir tank))]
;    (doto g
;      (.setColor
;                 (get-tank-color tank)) 
;      (.fillOval g x y d d)
;      (.drawLine (+ hx (* x scale)) (+ hy (* y scale)) 
;                (+ tx (* x scale)) (+ ty (* y scale))))))

(defn render-place [g p x y]
  "get a cell from the world and check if it has a tank.
  If it has: render it"
  (when (and (:tank p) (not= (:tank p) 0))
    (render-a-tank (:tank p) g x y)))


(defn render [g]
  "render the world by iterating over all its cells"
  (let [v (dosync (apply vector (for [x (range dim) y (range dim)] 
                                   @(place [x y]))))
        img (new BufferedImage (* scale dim) (* scale dim) 
                 (. BufferedImage TYPE_INT_ARGB))
        bg (. img (getGraphics))]
    (doto bg
      (.setColor (. Color white))
      (.fillRect 0 0 (. img (getWidth)) (. img (getHeight))))
    (dorun 
     (for [x (range dim) y (range dim)]
       (render-place bg (v (+ (* x dim) y)) x y)))
    (. g (drawImage img 0 0 nil))
    (. bg (dispose))))

(def panel (doto (proxy [JPanel] []
                        (paint [g] (render g)))
             (.setPreferredSize (new Dimension 
                                     (* scale dim) 
                                     (* scale dim)))))

(def frame (doto (new JFrame) (.add panel) .pack .show))
(def animator (agent nil))

(defn animation [x]
  "the animation agent"
  (when running
    (send-off *agent* #'animation))
  (. panel (repaint))
  (. Thread (sleep animation-sleep-ms))
  nil)

(defn tank-behaviour
  "the basic behaviour of a tank"
  [loc]
  (dosync
    (when running
      (send-off *agent* #'tank-behaviour))
    (move loc)))
