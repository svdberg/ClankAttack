(ns ClankAttack.core)

(import '(javax.swing JFrame JPanel)
        '(java.awt Color Graphics Dimension)
        '(java.awt.image BufferedImage))

; bunch of constants
(def *field-width* 500)
(def *field-height* 500)
(def *tank-radius* 10)

(defrecord Tank [x y id angle])

(defn create-tank
  "Randomly create tank"
  []
  (Tank. (rand-int *field-width*) (rand-int *field-height*) :foo
         (rand-int 360)))

(defn create-tanks
  "Randomly place n non-overlapping tanks"
  [n]
  (repeatedly n create-tank))

(defn render [g]
  (let [img (new BufferedImage *field-width* *field-height*
                 (. BufferedImage TYPE_INT_ARGB))
        bg (.getGraphics img)]
    (doto bg
      (.setColor (Color. 255 230 255))
      (.fillRect 0 0 (.getWidth img) (.getHeight img)))
    (.drawImage g img 0 0 nil)
    (.dispose bg)))

(defn create-panel [tanks]
  (doto (proxy [JPanel] []
          (paint [g] (render g)))
    (.setPreferredSize (new Dimension *field-width* *field-height*))))

(defn draw-battle-field [tanks]
  (doto (new JFrame "Clank Attack")
    (.add (create-panel tanks))
    .pack
    .show))
