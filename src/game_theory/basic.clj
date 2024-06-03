(ns game-theory.basic)

(def initial-state
  {:first-player []
   :second-player []
   :score {:first-player 0
           :second-player 0}})


(defn- grudger
  [current-state player-key opponent-key]
  (let [opponent-moves (opponent-key current-state)]
    (if (some #(= :de %) opponent-moves)
      :de
      :co)))


(defn- not-so-simple
  [current-state player-key opponent-key]
  (if (> (rand-int 10) 2)
    :de
    :co))


(defn- simple
  [current-state player-key opponent-key]
  :co)


(defn- devil
  [current-state player-key opponent-key]
  :de)


(defn- tit-for-tat
  [current-state player-key opponent-key]
  (let [opponent-moves (opponent-key current-state)]
    (or (last opponent-moves) :co)))


(defn- joss
  [current-state player-key opponent-key]
  (let [opponent-moves (opponent-key current-state)]
    (if (> (rand-int 10) 2)
      :de
      (or (last opponent-moves) :co))))


(defn- compute-state
  [current-state first-player-move second-player-move]
  (let [state-with-updated-moves
        {:first-player (conj (:first-player current-state)
                             first-player-move)
         :second-player (conj (:second-player current-state)
                              second-player-move)}]
    (cond
      ;; Both cooperate
      (and (= :co first-player-move)
           (= :co second-player-move))
      (assoc state-with-updated-moves
             :score {:first-player (-> current-state
                                       :score :first-player
                                       (+ 3))
                     :second-player (-> current-state
                                        :score :second-player
                                        (+ 3))})

      ;; Both defect
      (and (= :de first-player-move)
           (= :de second-player-move))
      (assoc state-with-updated-moves
             :score {:first-player (-> current-state
                                       :score :first-player
                                       (+ 1))
                     :second-player (-> current-state
                                        :score :second-player
                                        (+ 1))})

      ;; One cooperatiion one defection
      (and (= :co first-player-move)
           (= :de second-player-move))
      (assoc state-with-updated-moves
             :score {:first-player (-> current-state
                                       :score :first-player
                                       (+ 0))
                     :second-player (-> current-state
                                        :score :second-player
                                        (+ 5))})

      (and (= :de first-player-move)
           (= :co second-player-move))
      (assoc state-with-updated-moves
             :score {:first-player (-> current-state
                                       :score :first-player
                                       (+ 5))
                     :second-player (-> current-state
                                        :score :second-player
                                        (+ 0))}))))


(defn- simulate-game
  [rounds-number first-player-stragey second-player-stragey]
  (loop [state initial-state
         n rounds-number]
    (let [first-player-move (first-player-stragey state
                                                  :first-player
                                                  :second-player)
          second-player-move (second-player-stragey state
                                                    :second-player
                                                    :first-player)
          new-state (compute-state state
                                   first-player-move
                                   second-player-move)
          n (dec n)]
      (if (pos? n)
        (recur new-state n)
        new-state))))
