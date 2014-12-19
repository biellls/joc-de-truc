(ns truc-gui.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:use [truc-gui.deck]
        [truc-gui.hand]
        [truc-gui.table]
        [truc-gui.truc])
  (:import [truc_gui.deck Card]))

;;
;; constants
;;
(def height 650)
(def width 1000)
(def cardwidth 100)
(def cardheight 139)
(def upside-down "resources/images/spanish_deck/tapada.jpg")

;;
;; drawing functions
;;
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
        offset (+ cardwidth 10)
        x ([(- cx offset) cx (+ cx offset)] (dec n))
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

(defn draw-card-slot [path slot]
  {:pre [(or (= slot :card1) (= slot :card2))]}
  (let [posx (- (/ width 2) (/ cardwidth 2))
        posy1 (- (* 11 (/ height 20)) (/ cardheight 2))
        posy2 (- (* 5 (/ height 20)) (/ cardheight 2))
        posy (if (= :card1 slot) posy1 posy2)
        ;rotation (rand 0.3)
        rotation 0.1]
    (q/with-translation [posx posy]
     (q/with-rotation [rotation]
       (draw-card path)))))

(defn draw-slots []
  (let [swidth 130
        sheight 190
        posx (/ width 2)
        posy1 (* 11 (/ height 20))
        posy2 (* 5 (/ height 20))
        draw-slot (fn [x y] (q/ellipse x y swidth sheight))]
    (draw-slot posx posy1)
    (draw-slot posx posy2)))

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

;;
;; quil functions
;;
(defn setup []
  (q/frame-rate 10)
  ; set color mode to hsb (hsv) instead of default rgb.
  (q/color-mode :hsb)
  ; setup function returns initial state. it contains
  ; circle color and position.
  (let [nplayers 2
        ncards 3
        [h1 h2] (->> (shuffle deck) (deal nplayers ncards))]
   {:img (q/load-image "resources/images/spanish_deck/tapada.jpg")
    :table ()
    :p1 (Player. :card1 h1 0)
    :p2 (Player. :card1 h2 0)}))

(defn update [state]
  ; todo check if update function is needed (not likely)
  state)


(defn draw [state]
  (q/background 140)
  (q/fill 49 139 87)
  (draw-slots)
  (draw-deck :p1)
  (draw-card-slot lamo :card1)
  (draw-my-hand (get-in state [:p1 :hand]))
  )

(q/defsketch truc-gui
  :title "truc de 2"
  :size [width height]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update is called on each iteration before draw is called.
  ; it updates sketch state.
  :update update
  :draw draw
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
