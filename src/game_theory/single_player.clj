(ns game-theory.single-player)

(def initial-state
  {:first-player-moves []
   :second-player-moves []
   :score {:first-player 0
           :second-player 0}})


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


(defn- simulate-game
  [rounds-number first-player-stragey second-player-stragey]
  (loop [state initial-state
         n rounds-number]
    (let [first-player-moves (:first-player-moves state)
          second-player-moves (:second-player-moves state)
          first-player-move (first-player-stragey first-player-moves
                                                  second-player-moves)
          second-player-move (second-player-stragey second-player-moves
                                                    first-player-moves)
          [first-player-score second-player-score]
          (compute-score first-player-move
                         second-player-move)

          new-state (-> state
                        (update :first-player-moves conj first-player-move)
                        (update :second-player-moves conj second-player-move)
                        (update-in [:score :first-player] + first-player-score)
                        (update-in [:score :second-player] + second-player-score))

          n (dec n)]
      (if (pos? n)
        (recur new-state n)
        new-state))))

(comment

  (simulate-game 10 joss tit-for-tat)

  ;; ---

  (-> 10
      (simulate-game joss tit-for-tat)
      :score))
