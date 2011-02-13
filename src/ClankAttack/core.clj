(ns ClankAttack.core)

(import '(javax.swing JFrame JPanel)
        '(java.awt Color Graphics Dimension)
        '(java.awt.image BufferedImage))

; bunch of constants
(def *field-width* 500)
(def *field-height* 500)
(def *tank-radius* 10)

; tank stuff should be moved to tank.clj

(defrecord Tank [x y id angle])

(defn create-tank
  "Randomly create tank"
  []
  (Tank. (rand-int *field-width*) (rand-int *field-height*)
         (if (zero? (rand-int 2)) :friend :foo)
         (rand-int 360)))

(defn create-tanks
  "Randomly place n non-overlapping tanks"
  [n]
  (repeatedly n create-tank))

(defn get-tank-color [tank]
  (if (= (:id tank) :friend) (Color/green) (Color/blue)))

(defn grad-to-rad [grad]
  (* (/ grad 180) Math/PI))

(defn render-body [g tank]
  (let [r *tank-radius*
        d (* r 2)
        x (- (:x tank) r)
        y (- (:y tank) r)]
    (.fillOval g x y d d)))
  
(defn render-barrel [g tank]
  (let [x1 (:x tank)
        y1 (:y tank)
        alfa (grad-to-rad (:angle tank))
        x2 (+ x1 (* (Math/cos alfa) 20))
        y2 (+ y1 (* (Math/sin alfa) 20))]
    (.drawLine g x1 y1 x2 y2)))

(defn render-tank
  "Render a single tank"
  [g tank]
  (.setColor g (get-tank-color tank))
  (render-body g tank)
  (render-barrel g tank))
  
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
