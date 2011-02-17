(ns ClankAttack.core
  (:use [ClankAttack.tank])
  (:use [ClankAttack.world])
  (:import (javax.swing JFrame JPanel))
  (:import (java.awt Color Graphics Dimension))
  (:import (java.awt.image BufferedImage)))

; bunch of constants
;
;pixels per world cell
(def scale 5)

; equals a grid world
(def *field-width* (* dim scale))
(def *field-height* (* dim scale))
  
(def running true)
(def animation-sleep-ms 100)
(def tank-sleep-ms 40)

(defn render-background [g img]
  (doto g
    (.setColor (Color. 255 230 255))
    (.fillRect 0 0 (.getWidth img) (.getHeight img))))

;new world drawing
(defn render-a-tank [tank #^Graphics g x y]
  (let [t (assoc tank :x (* x scale) :y (* y scale))]
    (render-tank g t)))

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

(defn behave
  "the basic behaviour of a tank"
  [loc]
  (let [ p (place loc)
         tank (:tank @p)
         ahead (place (delta-loc loc (:dir tank)))]
    (. Thread (sleep tank-sleep-ms))
    (dosync
      (when running
        (send-off *agent* #'behave))
      (cond
        (= (:tank @ahead) 0)
          (move loc)
        :else
          loc))))
