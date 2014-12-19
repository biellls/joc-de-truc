(ns truc-gui.deck
  (:gen-class))

;; A card is made up of a suit and a number
;; The number serves as the value of the card
(defrecord Card [number suit])
(defn make-card [number suit] (Card. number suit))
(defn print-card [card]
  (println (:number card) " de " (name (:suit card))))
(defn to-str-card [card]
  (if card
    (str (:number card) " de " (name (:suit card)))
    nil))

;; A deck is just a collection of cards
(defn- make-deck [suits numbers]
  (for [s suits
        n numbers]
    (Card. n s)))

;;
;; Current supported decks: spanish deck
;;
(def spanish-deck
  (let [suits [:oros :bastos :copas :espases]
        numbers (range 1 13)]
    (make-deck suits numbers)))

;;
;; Functions on decks
;;
(defn deal
  "Deals the specified number of cards to every player. The deck should already be
   shuffled when it's passed"
  [nplayers ncards deck]
  (take nplayers (partition ncards deck)))


(defn remove-numbers
  "Removes from the deck all cards which contain one of the given numbers"
  [nums deck]
  (remove #(some #{(:number %)} nums) deck))
