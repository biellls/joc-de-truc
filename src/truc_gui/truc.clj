(ns truc-gui.truc
  (:gen-class)
  (:use [truc-gui.deck])
  (:import [truc_gui.deck Card]))

;;;;
;; This file contains all game logic necessary to play the game
;;;;

;; Deck needed to play a game of truc: a Spanish deck with all 2s, 8s
;; and 9s removed
(def deck (->> spanish-deck (remove-numbers [2 8 9])))

;;
;; Special cards
;;
(def lamo (Card. 11 :bastos))
(def madona (Card. 10 :oros))
(def llengo-bona (Card. 1 :espases))
(def llengo-xereca (Card. 1 :bastos))
(def manilla-bona (Card. 7 :espases))
(def manilla-xereca (Card. 7 :oros))
;; List of all special cards from highest to lowest power (pessa/peces
;; = special card)
(def peces [lamo madona llengo-bona llengo-xereca manilla-bona manilla-xereca])

;;
;; Methods necessary to compare card power and decide which card wins
;;
(defn- pessa? [carta]
  (some #(= % carta) peces))
(defn- tres? [carta]
  (= (:number carta) 3))
(defn- as? [carta]
  (= (:number carta) 1))

(defn- highest-number
  "The card with the highest number wins. Draw if both are the same number
   TODO: use predicate logic"
  [carta1 carta2]
  {:pre [(not (pessa? carta1))
         (not (pessa? carta2))]}
  (cond (and (tres? carta1) (tres? carta2)) nil
        (tres? carta1) :p1
        (tres? carta2) :p2
        (and (as? carta1) (as? carta2)) nil
        (as? carta1) :p1
        (as? carta2) :p2
        (> (:number carta1) (:number carta2)) :p1
        (< (:number carta1) (:number carta2)) :p2
        :else nil))
(defn- pessa-major [carta1 carta2]
  {:pre [(pessa? carta1)
         (pessa? carta2)]}
  (let [pos1 (.indexOf peces carta1)
        pos2 (.indexOf peces carta2)]
    (if (< pos1 pos2)
      :p1
      :p2)))
(defn fight
  "Returns :p1 if player 1 wins, :p2 if player 2 wins and nil if draw"
  [carta1 carta2]
  (cond (not (or (pessa? carta1) (pessa? carta2))) (highest-number carta1 carta2)
        (not (pessa? carta1)) :p2
        (not (pessa? carta2)) :p1
        :else (pessa-major carta1 carta2)))

