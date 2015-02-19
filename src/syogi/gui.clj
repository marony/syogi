(ns syogi.gui
  (:use syogi.lib)
  (:import (javax.swing JFrame JPanel)
           (java.awt Container Dimension Graphics Color FontMetrics)))

; 一つの駒の大きさ
(def piece-size {:width 32, :height 32})
; フレームの大きさ
(def frame-size {:width (* (:width piece-size) (+ board-size 2)),
                 :height (* (:height piece-size) (+ board-size 2))})

;(defn create-color [^int red, ^int green, ^int blue]
(defn create-color [red, green, blue]
  "Color作成(^int指定したいので)"
  (Color. (int red) (int green) (int blue)))

(def colors {:board-color (create-color 255 255 0)})

(def current-game (atom nil))

(def panel
  (let [piece-x (:width piece-size)
        piece-y (:height piece-size)
        frame-x (:width frame-size)
        frame-y (:height frame-size)]
    (letfn [(draw-piece [graphics [x y :as position] {:keys [id type nari owner], :as piece}]
              "駒を描画"
              (let [draw-x (+ piece-x (* x piece-x))
                    draw-y (+ piece-y (* y piece-y) (.. graphics getFontMetrics getAscent))
                    aft (java.awt.geom.AffineTransform.)]
                (if (and nari (not (= type :OU)))
                  (.setColor graphics Color/RED)
                  (.setColor graphics Color/BLACK))
                (if (= owner :SECOND_MOVE)
                  (.setToRotation aft (double (java.lang.Math/toRadians 180))
                    (double (+ draw-x (/ (double piece-x) 3))) (double draw-y))
                  (.setToTranslation aft (/ (double piece-x) 3) (/ (double piece-y) 3)))
                (doto graphics
                  (.setTransform aft)
                  (.drawString ((piece-type nari) type) draw-x draw-y))))]
      (proxy [JPanel] []
        (paint [graphics]
          (doto graphics
            ; 盤面を塗りつぶす
            (.setColor (:board-color colors))
            (.fillRect 0 0 (:width frame-size) (:height frame-size))
            (.setColor Color/BLACK))
          ; 線を引く
          (let [cols (map #(+ (* % piece-x) piece-x) (range (+ board-size 1)))
                rows (map #(+ (* % piece-y) piece-y) (range (+ board-size 1)))]
            ; 縦線
            (doseq [x cols]
              (let [start-y piece-y
                    end-y (- frame-y piece-y)]
                (.drawLine graphics x start-y x end-y)))
            (doseq [y rows]
              (let [start-x piece-x
                    end-x (- frame-x piece-x)]
                (.drawLine graphics start-x y end-x y))))
          (if @current-game
            (let [{:keys [board turn]
                   {mochi-gomaF :FIRST_MOVE mochi-gomaS :SECOND_MOVE :as mochi-goma} :mochi-goma, :as game} @current-game]
              ; 盤面に駒を描く
              (doseq [[index piece] (map-indexed list board)]
                (if (not= (:id piece) 0)
                  (draw-piece graphics (get-position-from-index index) piece)))
              ; 持ち駒を描く(先手)
              (doseq [[index piece] (map-indexed list mochi-gomaF)]
                (if (not= (:id piece) 0)
                  (draw-piece graphics [board-size ( - board-size index)] piece)))
              ; 持ち駒を描く(後手)
              (doseq [[index piece] (map-indexed list mochi-gomaS)]
                (if (not= (:id piece) 0)
                  (draw-piece graphics [-1 index] piece)))
              )))))))

(defn draw-gui [game]
  "ゲームの状態が変わったので再描画"
  (reset! current-game game)
  (.repaint panel))

(def frame
  (JFrame.))

(defn initialize-frame []
  "将棋画面初期化"
  (.. frame getContentPane (setPreferredSize (Dimension. (:width frame-size) (:height frame-size))))
  (doto frame
    (.setDefaultCloseOperation
      javax.swing.WindowConstants/EXIT_ON_CLOSE)
    (.add panel)
    ;    (.setSize 640 480)
    (.setTitle "将棋")
    (.setResizable false)
    (.pack)
    (.setVisible true)))

