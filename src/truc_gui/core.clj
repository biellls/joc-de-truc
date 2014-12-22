(ns truc-gui.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:use [truc-gui.deck]
        [truc-gui.hand]
        [truc-gui.table]
        [truc-gui.truc]
        [clojure.test :only [is]])
  (:import [truc_gui.deck Card]))

;;
;; Game constants
;;
(def height 650)
(def width 1000)
(def cardwidth 100)
(def cardheight 139)
(def upside-down "resources/images/spanish_deck/tapada.jpg")

;;
;; Drawing functions
;;
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

(defn draw-deck [player]
  (let [startx 750
        starty (if (= player :p1) 450 50)]
    (dotimes [n 10]
      (let [x (+ startx n)
            y (- starty n)]
        (draw-card upside-down x y)))))

(defn draw-card-slot [card slot]
  {:pre [(or (= slot :card1) (= slot :card2))]}
  (let [posx (- (/ width 2) (/ cardwidth 2))
        posy1 (- (* 11 (/ height 20)) (/ cardheight 2))
        posy2 (- (* 5 (/ height 20)) (/ cardheight 2))
        posy (if (= :card1 slot) posy1 posy2)
        ;rotation (rand 0.3)
        rotation 0.1]
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

(defn draw-table [table]
  (doseq [s [:card1 :card2]]
   (when-let [card (top-card s table)]
     (draw-card-slot card s))))

;;
;; functions to deal with game state
;;
;; player: has a specified slot on the table, a hand of 3 cards and a score
(defrecord Player [slot hand score])
(defn empty-player [slot] (Player. slot [] 0))

;;
;; Auxiliary functions
;;
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
    (cond (insideh1 x y) (when (>= (count hand) 1) (nth hand 0))
          (insideh2 x y) (when (>= (count hand) 2) (nth hand 1))
          (insideh3 x y) (when (>= (count hand) 3) (nth hand 2))
          :else nil)))

(defn- change-turn [state]
  {:pre [(is (or (= (:turn state) :p1)
                 (= (:turn state) :p2)))]}
  (let [turn (:turn state)]
    (if (= turn :p1)
      (assoc state :turn :p2)
      (assoc state :turn :p1))))

(defn- my-place-card [state slot card]
  (assoc state :table (place-card (:table state) slot card)))

(defn- play-card [state player-key card]
  (let [table (:table state)
        hand (get-in state [player-key :hand])
        slot (get-in state [player-key :slot])]
    (-> state
        (assoc-in [player-key :hand] (drop-card card hand))
        (my-place-card slot card)
        (assoc :time 0)
        (change-turn))))

(defn- computer-play-card [state]
  {:pre [(not (empty? (get-in state [:p2 :hand])))]}
  (let [hand (get-in state [:p2 :hand])
        card (nth hand 0)]
    (play-card state :p2 card)))

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
   {:img (q/load-image "resources/images/spanish_deck/tapada.jpg")
    :table (empty-table)
    :p1 (Player. :card1 h1 0)
    :p2 (Player. :card2 h2 0)
    :turn :p1
    :time 0}))

(defn update [state]
  (if (= (:turn state) :p2)
    (if (< (:time state) 8)
      (assoc state :time (inc (:time state)))
      (-> state
         (computer-play-card)))
    state))

(defn draw [state]
  (q/background 140)
  (q/fill 49 139 87)
  (draw-slots)
  (draw-deck :p1)
  (draw-my-hand (get-in state [:p1 :hand]))
  (draw-table (:table state)))

(defn- process-clicked-object [state object]
  (let [hand (get-in state [:p1 :hand])]
    (if (is-card? object)
      (play-card state :p1 object)
      state)))

(defn process-click
  "Decides appropiate action according to mouse click"
  [state click]
  (if (and (= (:button click) :left) (= :p1 (:turn state)))
    (if-let [clicked-object (get-clicked (:x click) (:y click)
                                         (get-in state [:p1 :hand]))]
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
