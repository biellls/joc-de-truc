(ns truc-gui.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:use [truc-gui.deck]
        [truc-gui.hand]
        [truc-gui.table]
        [truc-gui.truc]
        [clojure.test :only [is]])
  (:import [truc_gui.deck Card]))

;;;;
;; Game constants
;;;;
(def height 650)
(def width 1000)
(def cardwidth 100)
(def cardheight 139)
(def upside-down "resources/images/spanish_deck/tapada.jpg")

;;;;
;; Drawing functions
;;;;
(def draw-card-offset (+ cardwidth 10))

(declare card-image)
(defmulti draw-card (fn [entity & args] (class entity)))
(defmethod draw-card java.lang.String
  ([path]
     (draw-card path 0 0))
  ([path x y]
     (q/image (q/load-image path) x y)))
(defmethod draw-card truc_gui.deck.Card
  ([card]
     (draw-card (card-image card)))
  ([card x y]
     (draw-card (card-image card) x y)))

(defn draw-my-card
  "given a card from player's hand, draws it on the screen"
  [n card]
  (let [cx (- (/ width 2) (/ cardwidth 2))
        x ([(- cx draw-card-offset) cx (+ cx draw-card-offset)] (dec n))
        y 500]
    (draw-card card x y)))

(defn draw-my-hand
  "given the player's hand, it draws it on the screen"
  [hand]
  (dotimes [n (count hand)]
    (draw-my-card (inc n) (peek-card (inc n) hand))))

(defn draw-deck
  "Draws the deck near the player that dealed the cards in this round"
  [player]
  (let [startx 750
        starty (if (= player :p1) 450 50)]
    (dotimes [n 10]
      (let [x (+ startx n)
            y (- starty n)]
        (draw-card upside-down x y)))))

(defn draw-card-slot [rotation card slot]
  {:pre [(or (= slot :card1) (= slot :card2))]}
  (let [posx (- (/ width 2) (/ cardwidth 2))
        posy1 (- (* 11 (/ height 20)) (/ cardheight 2))
        posy2 (- (* 5 (/ height 20)) (/ cardheight 2))
        posy (if (= :card1 slot) posy1 posy2)]
    (q/with-translation [posx posy]
     (q/with-rotation [rotation]
       (draw-card card)))))

(defn draw-slots []
  (let [swidth 130
        sheight 190
        posx (/ width 2)
        posy1 (* 11 (/ height 20))
        posy2 (* 5 (/ height 20))
        draw-slot (fn [x y] (q/ellipse x y swidth sheight))]
    (draw-slot posx posy1)
    (draw-slot posx posy2)))

;; TODO draw all cards
(defn draw-table [table]
  (draw-slots)
  (doseq [s [:card1 :card2]]
    (let [n (ncards-placed s table)]
      (doseq [i (reverse (range 1 (inc n)))]
        (draw-card-slot (peek-angle i s table) (peek-card-table i s table) s)))))

;;;;
;; functions to deal with game state
;;;;
;; player: has a specified slot on the table, a hand of 3 cards and a score
(defrecord Player [slot hand score])
(defn empty-player [slot] (Player. slot [] 0))

(defn- set-turn [player state]
  (if player
    (assoc state :turn player)
    state))
(defn- get-turn [state]
  (:turn state))
(defn- get-player [player-key state]
  (player-key state))
(defn- set-player [player-key player state]
  (assoc state player-key player))
(defn- get-time [state] (:time state))
(defn- set-time [time state]
  (assoc state :time time))
(defn- get-table [state] (:table state))
(defn- set-table [table state]
  (assoc state :table table))
(defn- get-slot [player-key state]
  (let [player (get-player player-key state)]
    (:slot player)))
(defn- get-hand [player-key state]
  (let [player (get-player player-key state)]
    (:hand player)))
(defn- set-hand [player-key hand state]
  (let [player (get-player player-key state)]
    (set-player player-key (assoc player :hand hand) state)))

;;;;
;; Auxiliary functions
;;;;
(defn card-image
  "Given a card, returns the card's image path"
  [card]
  (str "resources/images/spanish_deck/" (:number card) (name (:suit card)) ".jpg"))

(defn- inside
  "Constructs function that returns true if arguments x y passed are inside
   the rectangel formed by ux uy lx ly, where ux > lx uy > ly"
  [ux uy lx ly]
  {:pre [(is (> ux lx))
         (is (> uy ly))]}
  (fn [x y]
    (and (>= x lx) (<= x ux)
         (>= y ly) (<= y uy))))

(defn- get-clicked-card
  "Retuns the card that the player selected from the screen"
  [n hand]
  (when (>= (count hand) n)
    (peek-card n hand)))

(defn get-clicked
  "Given a coordinate, find which object was clicked. Nil if none
   Possible return values: first/second/third card from player's hand
                           :d (deck)
   TODO implement deck click"
  [x y hand]
  (let [uxh2 (+ (/ width 2) (/ cardwidth 2)) ;; Upper bound hand 2
        lxh2 (- (/ width 2) (/ cardwidth 2))
        uxh1 (- uxh2 draw-card-offset)
        lxh1 (- lxh2 draw-card-offset)
        uxh3 (+ uxh2 draw-card-offset)
        lxh3 (+ lxh2 draw-card-offset)
        uyh (+ 500 cardheight)
        lyh 500
        insideh1 (inside uxh1 uyh lxh1 lyh)
        insideh2 (inside uxh2 uyh lxh2 lyh)
        insideh3 (inside uxh3 uyh lxh3 lyh)]
    (cond (insideh1 x y) (get-clicked-card 1 hand)
          (insideh2 x y) (get-clicked-card 2 hand)
          (insideh3 x y) (get-clicked-card 3 hand)
          :else nil)))

(defn top-card-player [player-key state]
  (let [slot (get-slot player-key state)
        table (get-table state)]
    (top-card slot table)))

(defn- his-turn? [player state]
  (= (get-turn state) player))
(defn- ncards-played [player-key state]
  (let [slot (get-slot player-key state)
        table (get-table state)]
    (ncards-placed slot table)))
(defn- get-round [state]
  (get-round-table (get-table state)))
(defn- round-winner
  "Given a state, calculates which player wins the round"
  [state]
  {:pre [(let [ncards1 (ncards-played :p1 state)
               ncards2 (ncards-played :p2 state)
               round (get-round state)]
           (is (= ncards1 ncards2))
           (is (= ncards1 round)))]}
  (let [card1 (top-card-player :p1 state)
        card2 (top-card-player :p2 state)]
    (fight card1 card2)))
(defn- write-round-winner [winner state]
  (let [table (get-table state)]
    (set-table (write-winner winner table) state)))

(defn- change-turn
  "Changes turn to other player if necessary.
   If only one player has played a card this round, give the turn to the other
   player.
   If both have played, give the turn to the player with the winning card"
  [state]
  {:pre [(is (or (his-turn? :p1 state)
                 (his-turn? :p2 state)))]}
  (cond (< (ncards-played :p1 state)
           (ncards-played :p2 state)) (set-turn :p1 state)
        (< (ncards-played :p2 state)
           (ncards-played :p1 state)) (set-turn :p2 state)
        :else (let [winner (round-winner state)]
                (->> state
                     (set-turn winner)
                     (write-round-winner winner)))))

(defn- my-place-card
  "Places the card on the table with a random angle of inclination"
  [slot card state]
  (let [table (get-table state)
        rotation (- (rand 0.6) 0.3)]
    (set-table (place-card table slot card rotation) state)))

(defn- play-card [state player-key card]
  (let [table (get-table state)
        hand (get-hand player-key state)
        slot (get-slot player-key state)]
    (->> state
        (set-hand player-key (drop-card card hand))
        (my-place-card slot card)
        (set-time 0)
        (change-turn))))

(defn- computer-play-card [state]
  {:pre [(not (empty? (get-hand :p2 state)))]}
  (let [hand (get-hand :p2 state)
        card (peek-card 1 hand)]
    (play-card state :p2 card)))

(defn- update-computer
  "Makes a move if appropriate"
  [state]
  (if (his-turn? :p2 state)
    (let [current-time (get-time state)]
      (if (< current-time 8)
        (set-time (inc current-time) state)
        (-> state
            (computer-play-card)
            state)))
    state))

(defn- update-text [state]
  (let [header "Truc\n"
        winner (get-winner (get-table state))
        message (str header
                     (when winner
                       (str "Player " (name winner) " wins!!!")))]
    (assoc state :text message)))

(defn- advance-round [table winner]
  (->> table
       (write-winner winner)
       ;(clear-table)
       ))

(defn- fight-ready? [table]
  (let [round (get-round table)
        ncards1 (ncards-placed :card1 table)
        ncards2 (ncards-placed :card2 table)]
    (= round ncards1 ncards2)))
    

(defn- update-game-state
  "Performs various tasks to manage game state, such as fighting cards and
   advancing rounds"
  [state]
  (let [table (get-table state)]
   (if (fight-ready? table)
     (let [card1 (top-card :card1 table)
           card2 (top-card :card2 table)
           winner (fight card1 card2)]
       (set-table state (advance-round table winner)))
     state)))

;;
;; quil functions
;;
(defn setup []
  (q/frame-rate 8)
  ; set color mode to hsb (hsv) instead of default rgb.
  (q/color-mode :hsb)
  ; setup function returns initial state. it contains
  ; circle color and position.
  (let [nplayers 2
        ncards 3
        [h1 h2] (->> (shuffle deck) (deal nplayers ncards))]
   {:table (empty-table)
    :p1 (Player. :card1 h1 0)
    :p2 (Player. :card2 h2 0)
    :turn :p1
    :time 0
    :text "Truc\nP1P2"}))

(defn update [state]
  (-> state
      (update-computer)
      (update-text)))

(defn draw [state]
  (q/background 140)
  (q/fill 255 255 255)
  (q/text (:text state) 10 15)
  (q/fill 49 139 87)
  (draw-deck (get-turn state))
  (draw-my-hand (get-hand :p1 state))
  (draw-table (get-table state)))

(defn- process-clicked-object [state object]
  (let [hand (get-hand :p1 state)]
    (if (is-card? object)
      (play-card state :p1 object)
      state)))

(defn process-click
  "Decides appropiate action according to mouse click"
  [state click]
  (if (and (= (:button click) :left) (his-turn? :p1 state))
    (if-let [clicked-object (get-clicked (:x click) (:y click)
                                         (get-hand :p1 state))]
      (-> (process-clicked-object state clicked-object))
      state)
    state))

(q/defsketch truc-gui
  :title "truc de 2"
  :size [width height]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update is called on each iteration before draw is called.
  ; it updates sketch state.
  :update update
  :draw draw
  :mouse-clicked process-click
  ; this sketch uses functional-mode middleware.
  ; check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

;; (defn -main []
;;   (sketch
;;    :title "truc de 2"
;;    :size [width height]
;;    :setup setup
;;    :update update
;;    :draw draw
;;    :middleware [m/fun-mode]))

;;;;;Remaining tasks;;;;;;;;;;;;;
;;TODO refactor code
;;TODO make cards not disappear when new one is placed

