(ns truc-gui.table
  (:gen-class)
  (:use [clojure.test :only [is]]
        [truc-gui.deck :only [to-str-card]]))

;; A table contains two slots for players to place their cards
;; It also keeps track of the round winners (round can be inferenced
;; from the lis t of winners
(defrecord Table [card1 card2 winners])
(defn empty-table [] (Table. nil nil []))

(defn place-card [table slot card]
  {:pre [(is (#{:card1 :card2} slot))]}
  (assoc table slot card))
(defn clear-table [table] (assoc table :card1 nil :card2 nil))
(defn write-winner
  "Writes the specified player as the winner in the current round"
  [player table]
  {:pre [(is (< (count (:winners table)) 3))]}
  (assoc table :winners (conj (:winners table) player)))
(defn get-winner [table]
  (let [[p q] (distinct (:winners table))
        {fp p, fq q} (frequencies (:winners table))]
    (cond (empty? (:winners table)) nil
          (= 2 fp) p
          (= 2 fq) q
          :else nil)))
(defn get-round
  "By counting the number of round winners we know how many rounds have already
   been played. Therefore the current round must be 1 + nº of rounds played"
  [table]
  (inc (count (:winners table))))

(defn show-table [table]
  (println "Player 1's card " (to-str-card (:card1 table)))
  (println "Player 2's card " (to-str-card (:card2 table)))
  (println "Round " (get-round table)))
