(ns ClankAttack.core)

(import '(javax.swing JFrame JPanel)
        '(java.awt Color Graphics Dimension)
        '(java.awt.image BufferedImage))

; bunch of constants
(def *field-width* 500)
(def *field-height* 500)
(def *tank-radius* 20)

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

(defn render-tank
  "Render a single tank"
  [g tank]
  (doto g
    (.setColor (Color/green))
    (.fillOval (:x tank) (:y tank) *tank-radius* *tank-radius*)))

(defn render-background [g img]
  (doto g
    (.setColor (Color. 255 230 255))
    (.fillRect 0 0 (.getWidth img) (.getHeight img))))

(defn render-tanks
  "Render all tanks"
  [g tanks]
  (dorun (map #(render-tank g %) tanks)))

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
