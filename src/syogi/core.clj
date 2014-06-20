(ns syogi.core
  (:use syogi.lib)
  (:use syogi.gui)
  (:import (java.io PushbackReader FileReader)))

(set! *warn-on-reflection* true)

; 初期の盤面(9 x 9)
(def initial-board
  [
    ; 1行目
    (->Piece 19 :KYOUSU false :SECOND_MOVE) (->Piece 15 :KEIMA false :SECOND_MOVE) (->Piece 11 :GIN false :SECOND_MOVE)
    (->Piece 7 :KIN false :SECOND_MOVE) (->Piece 2 :OU true :SECOND_MOVE) (->Piece 8 :KIN false :SECOND_MOVE)
    (->Piece 12 :GIN false :SECOND_MOVE) (->Piece 16 :KEIMA false :SECOND_MOVE) (->Piece 20 :KYOUSU false :SECOND_MOVE)
    ; 2行目
    empty-piece (->Piece 3 :HISHA false :SECOND_MOVE) empty-piece
    empty-piece empty-piece empty-piece
    empty-piece (->Piece 5 :KAKU false :SECOND_MOVE) empty-piece
    ; 3行目
    (->Piece 23 :HU false :SECOND_MOVE) (->Piece 24 :HU false :SECOND_MOVE) (->Piece 25 :HU false :SECOND_MOVE)
    (->Piece 26 :HU false :SECOND_MOVE) (->Piece 27 :HU false :SECOND_MOVE) (->Piece 28 :HU false :SECOND_MOVE)
    (->Piece 29 :HU false :SECOND_MOVE) (->Piece 30 :HU false :SECOND_MOVE) (->Piece 31 :HU false :SECOND_MOVE)
    ; 4行目
    empty-piece empty-piece empty-piece
    empty-piece empty-piece empty-piece
    empty-piece empty-piece empty-piece
    ; 5行目
    empty-piece empty-piece empty-piece
    empty-piece empty-piece empty-piece
    empty-piece empty-piece empty-piece
    ; 6行目
    empty-piece empty-piece empty-piece
    empty-piece empty-piece empty-piece
    empty-piece empty-piece empty-piece
    ; 7行目
    (->Piece 32 :HU false :FIRST_MOVE) (->Piece 33 :HU false :FIRST_MOVE) (->Piece 34 :HU false :FIRST_MOVE)
    (->Piece 35 :HU false :FIRST_MOVE) (->Piece 36 :HU false :FIRST_MOVE) (->Piece 37 :HU false :FIRST_MOVE)
    (->Piece 38 :HU false :FIRST_MOVE) (->Piece 39 :HU false :FIRST_MOVE) (->Piece 40 :HU false :FIRST_MOVE)
    ; 8行目
    empty-piece (->Piece 6 :KAKU false :FIRST_MOVE) empty-piece
    empty-piece empty-piece empty-piece
    empty-piece (->Piece 4 :HISHA false :FIRST_MOVE) empty-piece
    ; 9行目
    (->Piece 21 :KYOUSU false :FIRST_MOVE) (->Piece 17 :KEIMA false :FIRST_MOVE) (->Piece 13 :GIN false :FIRST_MOVE)
    (->Piece 9 :KIN false :FIRST_MOVE) (->Piece 1 :OU false :FIRST_MOVE) (->Piece 10 :KIN false :FIRST_MOVE)
    (->Piece 14 :GIN false :FIRST_MOVE) (->Piece 18 :KEIMA false :FIRST_MOVE) (->Piece 22 :KYOUSU false :FIRST_MOVE)])

(defn draw-game [game]
  "ゲーム画面の描画"
  (draw-gui game))

(comment defn get-piece-string [{:keys [id type nari owner], :as piece}]
  "駒を描画するための文字列を取得"
  (str (owner-type owner)
    (if (not= id 0)
      ((piece-type nari) type)
      (apply str (repeat piece-width " ")))
    "|"))

(comment defn draw-game [{:keys [board]}]
  "ゲーム画面の描画"
  (letfn [(repeat-string [n s]
            "文字列を繰り返す"
            (apply str (repeat n s)))]
    (let [hyphen-string (repeat-string piece-width "-")
          separator-string (str (repeat-string 9 (str "+" hyphen-string)) "+")
          space-string (repeat-string piece-width " ")
          space-separator-string (str (repeat-string 9 (str "|" space-string)) "|")]
      (println separator-string)
      (letfn [(get-piece-string [{:keys [id type nari owner], :as piece}]
                ; 駒の表示
                (str (owner-type owner)
                  (if (not= id 0)
                    ((piece-type nari) type)
                    (apply str (repeat piece-width " ")))
                  "|"))
              (draw-line [line]
                ; 一行描画
                (do (println space-separator-string)
                  (print "|")
                  (doseq [piece line]
                    (print (get-piece-string piece)))
                  (println "")
                  (println separator-string)))]
        ; 全行描画
        (doseq [line (partition board-size board)]
          (draw-line line))))))

(defn play-game [{:keys [board turn]
                  {mochi-gomaF :FIRST_MOVE mochi-gomaS :SECOND_MOVE :as mochi-goma} :mochi-goma, :as game}
                 algos status]
  "ゲーム"
  (let [my-algo (algos turn)
        my-state (status turn)
        my-mochi-goma (mochi-goma turn)
        opposite (get-opposite-turn turn)]
    (draw-game game)
    (let [{:keys [piece move state]} ((:next-move my-algo) game my-state)]
      ; 駒を動かす(持ち駒も)
      (if-let [{{next-board :board, next-turn :turn, next-mochi-goma :mochi-goma, :as next-game} :game,
                old-piece :old-piece, next-position :position} (move-piece game piece move)]
        (do ; ルールと勝敗をチェック
          (if (check-rules game next-game old-piece piece move next-position)
            (if (check-victory game next-game old-piece piece move next-position)
              ; 勝敗が付いた
              (println turn "win!!")
              ; 再帰呼び出し(選手交代)
              (do (java.lang.Thread/sleep 500)
                (recur next-game algos (assoc status turn state))))
              (println turn "lose")))
        (println turn "lose")))))

(defn read-algorithm [name]
  "アルゴリズムのソースファイルを読み込む"
  (let [form (slurp (str name ".clj"))]
    (do (print "form = ")
      (prn form)
      (load-string form))))

(defn -main [& args]
  "将棋メイン"
  (if (= (count args) 2)
    (let [algo1 (read-algorithm (nth args 0))
          algo2 (read-algorithm (nth args 1))
          ret1 (do (println algo1) ((:init algo1)))
          ret2 (do (println algo2) ((:init algo2)))]
      (initialize-frame)
      (play-game (->Game initial-board :FIRST_MOVE {:FIRST_MOVE [], :SECOND_MOVE []})
        {:FIRST_MOVE algo1, :SECOND_MOVE algo2} {:FIRST_MOVE (:state ret1), :SECOND_MOVE (:state ret2)}))
    (println "USAGE: lein run -- src/syogi/test-algo1 src/syogi/test-algo2")))
