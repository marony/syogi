(ns syogi.lib)
;(ns syogi.lib
;  (:use [clojure.contrib seq]))

; 盤面の大きさ
(def board-size 9)
; 駒の表示幅
(def piece-width 4)
; 全部の駒の数
(def all-piece-count 40)

; 駒の種類(piece type)
;(def piece-type {false {:HU "HU", :KYOUSU "KY", :KEIMA "KE", :GIN "GI", :KIN "KI", :KAKU "KA", :HISHA "HI", :OU "OU"}
;                 true  {:HU "hu", :KYOUSU "ke", :KEIMA "ke", :GIN "gi", :KIN "ki", :KAKU "ka", :HISHA "hi", :OU "ou"}})
(def piece-type {false {:HU "歩", :KYOUSU "香", :KEIMA "桂", :GIN "銀", :KIN "金", :KAKU "角", :HISHA "飛", :OU "王"}
                 true  {:HU "と", :KYOUSU "杏", :KEIMA "圭", :GIN "全", :KIN "金", :KAKU "馬", :HISHA "竜", :OU "玉"}})

; 先攻・後攻
(def owner-type {:FIRST_MOVE "A:", :SECOND_MOVE "B:"})
;(def owner-type {:FIRST_MOVE "先", :SECOND_MOVE "後"})

; アルゴリズムの引数
;{:game game :state state}
; アルゴリズムの戻り値
;{:piece piece :move move :state state}

; 駒レコード
; id = 1 : 王, 2 : 玉, 3～4 : 飛車, 5～6 : 角, 7～10 : 金, 11～14 : 銀, 15～18 : 桂馬, 19～22 : 香子, 23～40 : 歩
; type = piece-type
; nari = false : 成っていない, true : 成り
; owner = owner-type
(defrecord Piece [id type nari owner])
(def empty-piece (->Piece 0 nil nil nil))

; 方向(上が前)
(def direction {:FORWARD [0 -1], :FORWARD-RIGHT [1 -1], :RIGHT [1 0], :BACKWARD-RIGHT [1 1],
                :BACKWARD [0 1], :BACKWARD-LEFT [-1 1], :LEFT [-1 0], :FORWARD-LEFT [-1 -1],
                :KEIMA-LEFT [-1 -2], :KEIMA-RIGHT [1 -2]})

; 駒の動き
; 持ち駒の場合はdirに座標([x y])が入る
(defrecord Move [dir step])

; 駒ごとの動き
(def kin-move {:FORWARD (->Move :FORWARD 1), :FORWARD-RIGHT (->Move :FORWARD-RIGHT 1),
               :RIGHT (->Move :RIGHT 1), :BACKWARD (->Move :BACKWARD 1),
               :LEFT (->Move :LEFT 1), :FORWARD-LEFT (->Move :FORWARD-LEFT 1)})
(def piece-move { false {:HU {:FORWARD (->Move :FORWARD 1)}, :KYOUSU {:FORWARD (->Move :FORWARD nil)},
                         :KEIMA {:KEIMA-LEFT (->Move :KEIMA-LEFT 1), :KEIMA-RIGHT (->Move :KEIMA-RIGHT 1)},
                         :GIN {:FORWARD (->Move :FORWARD 1), :FORWARD-RIGHT (->Move :FORWARD-RIGHT 1),
                               :BACKWARD-RIGHT (->Move :BACKWARD-RIGHT 1), :BACKWARD-LEFT (->Move :BACKWARD-LEFT 1),
                               :FORWARD-LEFT (->Move :FORWARD-LEFT 1)},
                         :KIN kin-move,
                         :KAKU {:FORWARD-RIGHT (->Move :FORWARD-RIGHT nil), :BACKWARD-RIGHT (->Move :BACKWARD-RIGHT nil),
                                :BACKWARD-LEFT (->Move :BACKWARD-LEFT nil), :FORWARD-LEFT (->Move :FORWARD-LEFT nil)},
                         :HISHA {:FORWARD (->Move :FORWARD nil), :RIGHT (->Move :RIGHT nil),
                                 :BACKWARD (->Move :BACKWARD nil), :LEFT (->Move :LEFT nil)},
                         :OU {:FORWARD (->Move :FORWARD 1), :FORWARD-RIGHT (->Move :FORWARD-RIGHT 1),
                              :RIGHT (->Move :RIGHT 1), :BACKWARD-RIGHT (->Move :BACKWARD-RIGHT 1),
                              :BACKWARD (->Move :BACKWARD 1), :BACKWARD-LEFT (->Move :BACKWARD-LEFT 1),
                              :LEFT (->Move :LEFT 1), :FORWARD-LEFT (->Move :FORWARD-LEFT 1)}}
                  true {:HU kin-move, :KYOUSU kin-move, :KEIMA kin-move, :GIN kin-move, :KIN nil,
                        :KAKU {:FORWARD (->Move :FORWARD 1), :FORWARD-RIGHT (->Move :FORWARD-RIGHT nil),
                               :RIGHT (->Move :RIGHT 1), :BACKWARD-RIGHT (->Move :BACKWARD-RIGHT nil),
                               :BACKWARD (->Move :BACKWARD 1), :BACKWARD-LEFT (->Move :BACKWARD-LEFT nil),
                               :LEFT (->Move :LEFT 1), :FORWARD-LEFT (->Move :FORWARD-LEFT nil)},
                        :HISHA {:FORWARD (->Move :FORWARD nil), :FORWARD-RIGHT (->Move :FORWARD-RIGHT 1),
                                :RIGHT (->Move :RIGHT nil), :BACKWARD-RIGHT (->Move :BACKWARD-RIGHT 1),
                                :BACKWARD (->Move :BACKWARD nil), :BACKWARD-LEFT (->Move :BACKWARD-LEFT 1),
                                :LEFT (->Move :LEFT nil), :FORWARD-LEFT (->Move :FORWARD-LEFT 1)},
                        :OU {:FORWARD (->Move :FORWARD 1), :FORWARD-RIGHT (->Move :FORWARD-RIGHT 1),
                             :RIGHT (->Move :RIGHT 1), :BACKWARD-RIGHT (->Move :BACKWARD-RIGHT 1),
                             :BACKWARD (->Move :BACKWARD 1), :BACKWARD-LEFT (->Move :BACKWARD-LEFT 1),
                             :LEFT (->Move :LEFT 1), :FORWARD-LEFT (->Move :FORWARD-LEFT 1)}}})


; ゲームの状態
; board = 9x9[Piece]
; turn = :FIRST_MOVE or :SECOND_MOVE
; mochi-goma = {:FIRST_MOVE [Pieces], :SECOND_MOVE [Pieces]}
(defrecord Game [board turn mochi-goma])

(defn find-first [pred coll]
  "コレクションから関数がtrueになる最初の要素を返す"
  (first (filter pred coll)))

(defn get-index-from-position [[x y]]
  "位置([x y])からインデックスを取得する"
  (if (or (nil? x) (nil? y)
        (< x 0) (>= x board-size)
        (< y 0) (>= y board-size))
    nil
    (+ (* y board-size) x)))

(defn get-position-from-index [index]
  "インデックスから位置([x y])を取得する"
  (if (or (nil? index) (< index 0) (>= index (* board-size board-size)))
    nil
    [(rem index board-size) (quot index board-size)]))

(defn get-index-from-board-by-id [board id]
  "盤面から位置を取得"
  (loop [index 0
         [head & tail] board]
    (if head
      (if (= (:id head) id)
        index
        (recur (+ index 1) tail))
      nil)))

(defn get-piece-from-board [board position]
  "盤面から駒を取得"
  (let [index (get-index-from-position position)]
    (if (or (nil? index) (< index 0) (>= index (* board-size board-size)))
      nil
      (nth board index))))

(defn get-piece-from-mochi-goma [mochi-goma id]
  "持ち駒から駒を探す"
  (find-first #(= (:id %) id) mochi-goma))

(defn get-piece-from-mochi-goma_by_type [mochi-goma type]
  "持ち駒から駒を探す"
  (find-first #(= (:type %) type) mochi-goma))

(defn translate-move [owner [x y :as direction]]
  "先攻か後攻かによって方向を回転する"
  (if (= owner :FIRST_MOVE)
    direction
    [(* x -1) (* y -1)]))

(defn is-nari-area [owner type [x y :as position]]
  "成れる場所か？"
  (if (= type :OU)
    false
    (if ((piece-type true) type)
      (if (= owner :FIRST_MOVE)
        (and (<= y 2) (>= y 0))
        (and (<= y 8) (>= y 6))))))

(defn get-moved-position [turn
                          [x y :as position]
                          {:keys [dir step], :as move}]
  "位置に移動を加算して位置を返す"
  (if (or (nil? turn) (nil? x) (nil? y)
        (nil? dir) (nil? step))
    nil
    (let [[move-x move-y] (translate-move turn (direction dir))
          [moved-x moved-y :as moved-position] [(+ x (* move-x step)) (+ y (* move-y step))]]
      (if (and (>= x 0) (< x board-size)
            (>= y 0) (< y board-size))
        moved-position
        ))))

(defn get-all-piece-count [{:keys [board turn] {mochi-gomaF :FIRST_MOVE mochi-gomaS :SECOND_MOVE :as mochi-goma} :mochi-goma, :as game}]
  "全ての駒の数を求める"
  (reduce #(+ %1 (count %2)) 0 [(filter #(not= % empty-piece) board) mochi-gomaF mochi-gomaS]))

(defn get-piece-count [turn coll]
  "先攻か後攻の駒の数を求める"
  (count (filter #(= (:turn %) turn) coll)))

(defn check-rules [{:keys [board turn] {mochi-gomaF :FIRST_MOVE, mochi-gomaS :SECOND_MOVE, :as mochi-goma} :mochi-goma, :as game}
                   {next-board :board, next-turn :turn,
                    {next-mochi-gomaF :FIRST_MOVE next-mochi-gomaS :SECOND_MOVE :as next-mochi-goma} :mochi-goma, :as next-game}
                   { old-id :id, old-type :type, old-nari :nari, old-owner :owner, :as old-piece}
                   piece, {:keys [dir step], :as move}, next-position]
  "ルールをチェックする(ルール違反ならfalse)"
  (letfn [(check-piece-count []
            "駒の数チェック(盤面と持ち駒の数を全部調べて、40個か？)"
            (if (and (= (get-all-piece-count game) all-piece-count)
                  (= (get-all-piece-count next-game) all-piece-count))
              true))
          (check-piece-count-difference []
            "駒の差異チェック"
            ; 駒の差異チェック(盤面のturnの数とturnの持ち駒が変わっていないかが変わっていないか)
            (if (or (and (= (get-piece-count turn board)
                           (get-piece-count turn next-board))
                      (= (get-piece-count turn (mochi-goma turn))
                        (get-piece-count turn (next-mochi-goma turn))))
                  ; 駒の差異チェック(盤面のnext-turnの駒が1つ減ってturnの持ち駒が1つ増えているか)
                  (and (= (get-piece-count next-turn board)
                         (get-piece-count next-turn (- next-board 1)))
                    (= (get-piece-count turn (mochi-goma turn))
                      (get-piece-count turn (+ (next-mochi-goma turn)))))
                  ; 駒の差異チェック(turnの持ち駒が1つ減って盤面のturnの駒が1つ増えているか)
                  (and (= (get-piece-count turn board)
                         (get-piece-count turn (+ next-board 1)))
                    (= (get-piece-count turn (mochi-goma turn))
                      (get-piece-count turn (- (next-mochi-goma turn))))))
              true))
          (check-nari []
            "駒の成りチェック"
            (if (not (seq? dir))
              ; TODO: 成りチェック
              true
              )
            )]
    ; 駒の数チェック(盤面と持ち駒の数を全部調べて、40個か？)
    (if (and (check-piece-count)
          (check-piece-count-difference)
          (check-nari)
          ; TODO: 二歩チェック
          ; TODO: 進めない駒チェック
          ; TODO: 千日手チェック
          ; TODO: 千日手(王手)チェック
          ; TODO: 持将棋チェック
          ; TODO: 打ち歩詰めチェック
          )
      (do println "check-rules"
        true))))

(defn check-victory [{:keys [board turn] {mochi-gomaF :FIRST_MOVE, mochi-gomaS :SECOND_MOVE, :as mochi-goma} :mochi-goma, :as game}
                     {next-board :board, next-turn :turn,
                      {next-mochi-gomaF :FIRST_MOVE next-mochi-gomaS :SECOND_MOVE :as next-mochi-goma} :mochi-goma, :as next-game}
                     old-piece piece move next-position]
  "勝敗をチェックする(勝敗が付いたらfalse)"
  ; 持ち駒に王があったら勝ち
  (let [my-mochi-goma (next-mochi-goma turn)]
    ; 持ち駒からも移動する
    (if (get-piece-from-mochi-goma_by_type my-mochi-goma :OU)
      true
      )))

; 駒の移動先の位置を取得
; 移動できない場合はnil
(defn get-move-target-position [{:keys [board turn] {mochi-gomaF :FIRST_MOVE mochi-gomaS :SECOND_MOVE :as mochi-goma} :mochi-goma, :as game}
                                {:keys [id type nari owner], :as piece}
                                {:keys [dir step], :as move}]
  "駒の移動先の位置を取得"
  ; 持ち駒か盤面にある駒か
  (if (seq? dir)
    ; 持ち駒に入っているかチェック
    (if (some #(= (% :id) id) dir)
      ; そのまま座標を返す
      dir)
    ; 駒の種類から動きを取得\
    (if (or (nil? (piece-move nari)) (nil? ((piece-move nari) type)))
      nil
      (let [{check-dir :dir, check-step :step, :as check-move} (((piece-move nari) type) dir)]
        ; 動かせるかチェック
        (if (and check-step (not= check-step step))
          ; 動かせないよ
          nil
          ; 移動先の座標を計算
          (let [[x y :as position] (get-position-from-index
                                     (get-index-from-board-by-id board id))]
            (get-moved-position turn position move)))))))

(defn get-opposite-turn [turn]
  "相手のターンを返す"
  (if (= turn :FIRST_MOVE)
    :SECOND_MOVE
    :FIRST_MOVE))

(defn is-piece-exists-between
  [board [start-x start-y :as start] [end-x end-y :as end]]
  "間にある駒を返す"
  (if (= start-x end-x)
    (if (< start-y end-y)
      (some #(true? %) (map #(get-piece-from-board board [start-x %]) (range (+ start-y 1) end-y)))
      (some #(true? %) (map #(get-piece-from-board board [start-x %]) (range (+ end-y 1) start-y)))))
  (if (= start-y end-y)
    (if (< start-x end-x)
      (some #(true? %) (map #(get-piece-from-board board [% start-y]) (range (+ start-x 1) end-x)))
      (some #(true? %) (map #(get-piece-from-board board [% start-y]) (range (+ end-x 1) start-x))))))

(defn replace-piece [{:keys [board turn] {mochi-gomaF :FIRST_MOVE mochi-gomaS :SECOND_MOVE} :mochi-goma, :as game}
                     {:keys [id type nari owner], :as piece}
                     position]
  "盤面の駒を入れ替える。戻り値として盤面を返す"
  (let [index1 (get-index-from-position position)]
    (loop [index 0
           [head & tail] board
           result-board []]
      (if head
        (recur (+ index 1) tail (conj result-board
                                  (if (= index index1)
                                    piece
                                    head)))
        result-board))))

(defn move-piece [{:keys [board turn] {mochi-gomaF :FIRST_MOVE mochi-gomaS :SECOND_MOVE :as mochi-goma} :mochi-goma, :as game}
                  {:keys [id type nari owner], :as piece}
                  {:keys [dir step], :as move}]
  "駒を移動し、盤面と持ち駒を変更し、新しい盤面を生成して返す"
  (let [my-mochi-goma (mochi-goma turn)
        opposite (get-opposite-turn turn)]
    ; 持ち駒からも移動する
    (if (seq? dir)
      (let [; 元の位置
            index (get-index-from-board-by-id board id)
            old-position (get-position-from-index index)
            ; 元の駒
            old-piece (get-piece-from-mochi-goma my-mochi-goma id)
            ; 駒の移動先を取得
            next-position dir]
        ; 持ち駒からの移動
        ; 移動先をチェック(どちらかの駒がいたら移動不可)
        (if (= old-piece empty-piece)
          (let [; TODO: 自分の持ち駒から駒を削除
                after-mochi-goma mochi-goma
                ; 新しい位置に置く
                next-board (replace-piece game piece next-position)]
            ; 盤面と持ち駒を更新
            {:game (->Game next-board opposite (assoc mochi-goma turn after-mochi-goma)),
             :old-piece old-piece,
             :position next-position})
          (println "piece already placed in move position")))
      ;盤面からの移動
      (let [; 元の位置
            index (get-index-from-board-by-id board id)
            old-position (get-position-from-index index)
            ; 元の駒
            old-piece (get-piece-from-board board old-position)
            ; 駒の移動先を取得
            next-position (get-move-target-position game piece move)]
        ; 間に駒があったら移動不可
        (if (and next-position (not (is-piece-exists-between board old-position next-position)))
          (do
            ; 移動先をチェック(自分の駒がいたら移動不可)
            (let [{next-old-id :id, next-old-type :type, next-old-nari :nari, next-old-owner :owner, :as next-old-piece}
                  (get-piece-from-board board next-position)]
              ; 自分の駒が移動先にいたら移動しない
              (if (= next-old-owner owner)
                (println "your piece already placed in move position")
                (do
                  ; 相手の駒があったら持ち駒に加える
                  (def after-mochi-goma (if (= next-old-owner opposite)
                                          (conj my-mochi-goma (->Piece next-old-id next-old-type false turn))
                                          my-mochi-goma))
                  ; 古い位置を消す
                  (def tmp-board (replace-piece game empty-piece old-position))
                  ; 新しい位置に置く
                  (def next-board (replace-piece (->Game tmp-board turn mochi-goma) piece next-position))
                  ; 盤面と持ち駒を更新
                  {:game (->Game next-board opposite (assoc mochi-goma turn after-mochi-goma)),
                   :old-piece old-piece
                   :position next-position}))))
          (println "piece can't move"))))))
