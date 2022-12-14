(ns tic-tac-toe.core
  (:gen-class))

(defn player2char [i]
  (i {:x "X", :o "O"}))

(defn row2char [r] (char (+ r (int \a))))

(defn field2number [field]
  (hash-map :y (- (int (first (seq field))) (int \a) -1)
            :x (- (int (last (seq field))) (int \0))))

(defn winner-exists? [moves]
  (let [hor-f (filter (fn [x] (= 3 (last x)))
                       (frequencies (map (fn [x] (list (first x) (nth (last x) 0)))
                                         moves)))

        ver-f (filter (fn [x] (= 3 (last x)))
                       (frequencies (map (fn [x] (list (first x) (nth (last x) 1)))
                                         moves)))

        diag1-f (->> moves
                    (map (fn [x] (into {:i (first x)} (field2number (last x)))))
                    (filter (fn [x] (= (:x x) (:y x))))
                    (map :i)
                    (frequencies)
                    (filter (fn [x] (= 3 (last x)))))]
    (cond
      (pos? (count hor-f)) (list true (player2char (first (first (first hor-f)))))
      (pos? (count ver-f)) (list true (player2char (first (first (first ver-f)))))
      (pos? (count diag1-f)) (list true (player2char (first (first diag1-f))))
      :else (list false))))

(defn move-played? [move moves]
  (let [f (filter (fn [e] (= (last e) move)) moves)]
    (pos? (count f))))

(defn field2str [move moves]
  (let [f (filter (fn [e] (= (last e) move)) moves)]
    (if (pos? (count f))
      (player2char (first (first f)))
      " ")))

(defn valid-move? [move moves]
  (cond
    (= move "q") true
    (= move :bad-move) false
    (move-played? move moves) false
    (re-matches #"[a-c][1-3]" move) true
    :else false))

(defn input-move [moves]
  (loop [move :bad-move
         first-time true]
    (if (valid-move? move moves)
      move
      (do
        (when (not first-time)
          (println "Illegal move"))
        (recur (read-line) false)))))

(defn dump-board [moves]
  (let [separator "+---+---+---+---+"]
    (println separator)
    (println "|   | 1 | 2 | 3 |")
    (println separator)
    (doseq [row (range 3)]
      (printf "| %c |" (row2char row))
      (doseq [column (range 3)]
        (printf " %s |" (field2str (str (row2char row) (inc column)) moves)))
      (printf "\n")
      (println separator))))

(defn play []
  (loop [moves '()
         player :x]
    (dump-board moves)
    (let [winner (winner-exists? moves)]
      (if (first winner)
        (do
          (println "Winner:" (last winner))
          (println "Moves: " (reverse moves))
          moves)
        (let [move (input-move moves)]
          (if (= move "q")
            moves
            (recur (conj moves (list player move))
                   (if (= player :x) :o :x))))))))

(play)
