(ns game-theory.multi-player)


(comment
  {:strategies {:suvrat stategy-fn}
   :moves {:suvrat {:pranav []
                    :parth []}
           :pranav {:suvrat []
                    :parth []}
           :parth {:suvrat []
                   :pranav []}}
   :score {:suvrat {:pranav 3
                    :parth 3}}
   :master-score {:suvrat 40
                  :pranav 50
                  :parth 60}})


(def initial-state
  {:strategies {}
   :moves {}
   :score {}
   :master-score {}})


(defn- modi-ji
  [player-moves opponent-moves]
  (if (even? (rand-int 2))
    :de
    :co))


(defn- grudger
  [player-moves opponent-moves]
  (if (some #(= :de %) opponent-moves)
    :de
    :co))


(defn- not-so-simple
  [player-moves opponent-moves]
  (if (> (rand-int 10) 2)
    :de
    :co))


(defn- simple
  [player-moves opponent-moves]
  :co)


(defn- devil
  [player-moves opponent-moves]
  :de)


(defn- tit-for-tat
  [player-moves opponent-moves]
  (or (last opponent-moves) :co))


(defn- tit-for-two-tats
  [player-moves opponent-moves]
  (let [last-move (last opponent-moves)
        second-last-move (-> opponent-moves
                             drop-last
                             last)]
    (if (and (= last-move :de)
             (= second-last-move :de))
      :de
      :co)))


(defn- joss
  [player-moves opponent-moves]
  (if (> (rand-int 10) 2)
    :de
    (or (last opponent-moves) :co)))


(defn- compute-score
  "Returns the score in this format:
   [<first-player-score> <second-player-score>]"
  [first-player-move second-player-move]
  (cond
      ;; Both cooperate
      (and (= :co first-player-move)
           (= :co second-player-move))
      [3 3]

      ;; Both defect
      (and (= :de first-player-move)
           (= :de second-player-move))
      [1 1]

      ;; One cooperatiion one defection
      (and (= :co first-player-move)
           (= :de second-player-move))
      [0 5]

      (and (= :de first-player-move)
           (= :co second-player-move))
      [5 0]))


(defn- simulate-round-for-player
  [state player opponents]
  (reduce (fn [state opponent]
            (let [player-moves (or (get-in state [:moves player opponent]) [])
                  opponent-moves (or (get-in state [:moves opponent player]) [])
                  player-strategy (get-in state [:strategies player])
                  opponent-strategy (get-in state [:strategies opponent])
                  player-move (player-strategy player-moves opponent-moves)
                  opponent-move (opponent-strategy player-moves opponent-moves)
                  [player-score opponent-score] (compute-score player-move
                                                               opponent-move)]
              (-> state
                  (update-in [:moves player opponent]
                             (fnil conj [])
                             player-move)
                  (update-in [:moves opponent player]
                             (fnil conj [])
                             opponent-move)
                  (update-in [:score player opponent]
                             (fnil + 0)
                             player-score)
                  (update-in [:score opponent player]
                             (fnil + 0)
                             opponent-score)
                  (update-in [:master-score player]
                             (fnil + 0)
                             player-score)
                  (update-in [:master-score opponent]
                             (fnil + 0)
                             opponent-score))))
          state
          opponents))


(defn- simulate-round
  [state players]
  (loop [state state
         [player & opponents]  players]
    (let [new-state (simulate-round-for-player state
                                               player
                                               opponents)]
      (if (seq opponents)
        (recur new-state opponents)
        state))))


(defn- simulate-game
  [rounds-number players]
  (let [initial-state {:strategies players}
        player-keys (sort (keys players))]
   (loop [state initial-state
          n rounds-number]
     (let [new-state (simulate-round state player-keys)
           n (dec n)]
       (if (pos? n)
         (recur new-state n)
         new-state)))))


(comment
  (-> 100
      (simulate-game {:tit-for-tat tit-for-tat
                      :tit-for-two-tats tit-for-two-tats
                      :grudger grudger
                      :joss joss
                      :simple simple
                      :modi-ji modi-ji
                      :not-so-simple not-so-simple
                      :devil devil})
      (select-keys [:score :master-score])))
