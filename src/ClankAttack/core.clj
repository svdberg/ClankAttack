(ns ClankAttack.core
  (:use [ClankAttack.tank])
  (:use [ClankAttack.world])
  (:import (javax.swing JFrame JPanel))
  (:import (java.awt Color Graphics Dimension))
  (:import (java.awt.image BufferedImage)))

; bunch of constants

;pixels per world cell
(def scale 10)

; equals a grid world
(def *field-width* (* dim scale))
(def *field-height* (* dim scale))
  
(def animation-sleep-ms 100)

(defn render-background [g img]
  (doto g
    (.setColor (Color. 255 230 255))
    (.fillRect 0 0 (.getWidth img) (.getHeight img))))

;world drawing
(defn render-a-tank [tank #^Graphics g x y]
  (let [x1 (* x scale)
        y1 (* y scale)
        hit (:shot tank)]
    (if hit nil (render-tank g x1 y1 tank))))

(defn render-wall [#^Graphics g x y]
  (let [ x1 (* x scale)
         y1 (* y scale)
         w scale
         h scale]
  (doto g
    (.setColor (. Color red))
    (.drawRect x1 y1 w h))))

(defn render-bullet [bullet #^Graphics g x y]
  (let [ r 6
         x1 (* x scale)
         y1 (* y scale)
         hit (:hit bullet)]
    (if hit nil (.fillOval g x1 y1 r r))))

(defn render-place [g p x y]
  "get a cell from the world and check if it has a tank.
  If it has: render it"
  (cond 
    (and (:tank p) (not= (:tank p) 0))
      (render-a-tank (:tank p) g x y)
    (= (:wall p) 1)
      (render-wall g x y)
    (and (:bullet p) (not= (:bullet p) 0))
      (render-bullet (:bullet p) g x y)))


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

(def wall-seq [(create-horizontal-wall 0 49 0)
        (create-horizontal-wall 0 49 49)
        (create-vertical-wall 1 49 0)
        (create-vertical-wall 1 49 49)
        (create-vertical-wall 10 40 15)
        (create-vertical-wall 10 40 40)])

(defn setup-walls
  []
  (doall wall-seq))

(defn demo
  []
  "Runs a demo
  setup tanks
  setup walls
  run agents"
  (let [walls (doall wall-seq)
        tanks (setup)
        ranimation (send-off animator animation)]
    (dorun (map #(send-off % behave) tanks))))

