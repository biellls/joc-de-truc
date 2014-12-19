(ns truc-gui.hand
  (:gen-class)
  (:use [clojure.test :only [is]]))

;;
;; A hand is a vector of 3 cards
;;
(defn peek-card
  "Peeks at card in position n"
  [n hand]
  {:pre [(is (<= n (count hand)))
         (is (> n 0))]}
  (nth hand (dec n)))

(defn drop-card
  "Removes card from hand"
  [card hand]
  {:pre [(is (some #{card} hand))]}
  (remove #(= card %) hand))
