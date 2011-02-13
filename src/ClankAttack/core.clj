(ns ClankAttack.core
  (:use [ClankAttack.tank])
  (:import (javax.swing JFrame JPanel))
  (:import (java.awt Color Graphics Dimension))
  (:import (java.awt.image BufferedImage)))

; bunch of constants
(def *field-width* 500)
(def *field-height* 500)
  
(defn render-background [g img]
  (doto g
    (.setColor (Color. 255 230 255))
    (.fillRect 0 0 (.getWidth img) (.getHeight img))))

(defn render [g tanks]
  (let [img (new BufferedImage *field-width* *field-height*
                 (. BufferedImage TYPE_INT_ARGB))
        bg (.getGraphics img)]
    (render-background bg img)
    (render-tanks bg tanks)
    (.drawImage g img 0 0 nil)
    (.dispose bg)))

(defn create-panel [tanks]
  (doto (proxy [JPanel] []
          (paint [g] (render g tanks)))
    (.setPreferredSize (new Dimension *field-width* *field-height*))))

(defn draw-battle-field [tanks]
  (doto (new JFrame "Clank Attack")
    (.add (create-panel tanks))
    .pack
    .show))
