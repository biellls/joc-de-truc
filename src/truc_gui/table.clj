(ns truc-gui.table
  (:gen-class)
  (:use [clojure.test :only [is]]
        [truc-gui.deck :only [to-str-card]]))

;; A table contains two slots for players to place their cards
;; It also keeps track of the round winners (round can be inferenced
;; from the list of winners)
(defrecord Table [card1 card2 winners])
(defn empty-table [] (Table. nil nil []))

(defn place-card
  "Places a card on the specified slot (several cards can be on the slot, but
   only the ones from the current round should be evaluated in a battle)
   The slot is represented as a list, so the current card is always the first"
  [table slot card]
  {:pre [(is (#{:card1 :card2} slot))
         (is (< (count (slot table)) 3))]}
  (assoc table slot (conj (slot table) card)))

(defn top-card
  "The slot is represented as a list, so the current card is always the first
   Responsibility to check that the topmost card indeed belongs to the current
   round is deferred to a higher level"
  [slot table]
  (first (slot table)))

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
