(ns syogi.algorithm
  (:use [syogi.lib]))

(def my-turn :SECOND_MOVE)

;; テストアルゴリズム1
(let []
  ; 初期化
  {:init (fn [] {:state 0})
   ; 駒を動かす
   :next-move (fn [{:keys [board turn]
                    {mochi-gomaF :FIRST_MOVE mochi-gomaS :SECOND_MOVE :as mochi-goma} :mochi-goma, :as game}
                   state]
                (let [my-board (filter #(= turn (:owner %)) board)]
                  (println "turn B" (:turn game))
                  ;                  (print "my-board = ")(prn my-board)
                  (loop []
                    ; ランダムで駒を選ぶ
                    (let [board-length (count my-board)
                          {:keys [id type nari owner], :as my-piece} (nth my-board (rand-int board-length))]
                      (print "piece = ")(prn my-piece)
                      (if-let [nari-move (piece-move nari)]
                        (let [my-moves (nari-move type)
                              keys (keys my-moves)
                              move-count (count keys)
                              key (nth keys (rand-int move-count))
                              my-move (my-moves key)
                              ]
                          (print "move-count = ")(prn move-count)
                          (print "key = ")(prn key)
                          (print "my-move = ")(prn my-move)
                          (let [{{next-board :board, next-turn :turn, next-mochi-goma :mochi-goma, :as next-game} :game,
                                 old-piece :old-piece, next-position :position} (move-piece game my-piece (->Move (:dir my-move) 1))]
                            (do ; ルールと勝敗をチェック
                              (if (and next-game (check-rules game next-game old-piece my-piece (->Move (:dir my-move) 1) next-position))
                                {:piece (->Piece (:id my-piece) (:type my-piece) (or nari (is-nari-area turn type next-position))
                                          (:owner my-piece)),
                                 :move (->Move (:dir my-move) 1), :state (+ state 1)}
                                (recur)))))
                        (recur))))))})
