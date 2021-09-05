(ns gol.core
  (:require [reagent.dom :as dom]
            [reagent.core :as r]
            [clojure.spec.alpha :as s]
            ["react-konva" :as k]))

(s/def ::cell #{:alive :dead})
(s/def ::board (s/coll-of (s/coll-of ::cell )))


(s/fdef toggle
  :args ::cell
  :ret ::cell)
(def toggle {:dead  :alive
             :alive :dead})

(defn alive? [cell] (= :alive cell))(def dead? (complement alive?))

(defn ->vec
  "converts a seq board into a vector board.
  `for` and other constructs represent things as seq's, not vectors"
  [board]
  (into [] (map (partial into [])) board))



(defn gen-empty-board [rows cols]
  (->> :dead
       (repeat cols)
       (repeat rows)
       (into [] (map (partial into [])))))

(defn gen-initial-board [rows cols]
  (let [empty-board (->> :dead
                         (repeat cols)
                         (repeat rows)
                         ->vec)]
    (-> empty-board
        (update-in [2 2] toggle)
        (update-in [3 2] toggle)
        (update-in [4 2] toggle)
        (update-in [4 1] toggle)
        (update-in [3 0] toggle))))

(defn get-neighbors [board [row col]]
  (for [drow  (range -1 2)
        dcol  (range -1 2)
        :when (not= 0 drow dcol)]
    (get-in board
            [(+ row drow) (+ col dcol)])))

(defn count-living-neighbors [board [row col]]
  (->> (get-neighbors board [row col])
       (filter alive?)
       count))

(defn board-map [f coll]
  (let [row-count (count coll)
        col-count (count (first coll))]    
    (->vec
     (for [row-idx (range row-count)]
       (for [col-idx (range col-count)]
         (f row-idx col-idx))))))

(defn tick [board]
  (board-map
   (fn [row-idx col-idx]
     (let [cell (get-in board [row-idx col-idx])
           neighbor-count
           (count-living-neighbors board [row-idx col-idx])]
       (cond
         (and (alive? cell)
              (or (= 2 neighbor-count)
                  (= 3 neighbor-count))) :alive

         (and (dead? cell)
              (= 3 neighbor-count)) :alive

         :else :dead)))
   board))

(defn game
  ([initial-board]
   (let [rows    (count initial-board)
         columns (count (first initial-board))]
     (game initial-board rows columns)))
  ([rows columns]
   (game (gen-initial-board rows columns) rows columns))
  ([initial-game-board rows columns]
   (let [state (r/atom initial-game-board)]
     (fn []
       (let [width (.-innerWidth js/window)
             height (.-innerHeight js/window)
             cell-edge-and-spacing-length (min (/ width columns)
                                               (/ (- height 30)
                                                  rows))

             cell-edge-length (* 0.9 cell-edge-and-spacing-length)

             canvas-width  (* cell-edge-and-spacing-length columns)
             canvas-height (* cell-edge-and-spacing-length rows)]

         [:div
          [:> k/Stage {:width canvas-width :height canvas-height}
           [:> k/Layer
            (map-indexed
             (fn [row-idx row]
               (map-indexed
                (fn [col-idx cell]
                  ^{:key (str [col-idx row-idx cell])}
                  [:> k/Rect {:key (str [col-idx row-idx cell])
                              :x (* col-idx cell-edge-and-spacing-length)
                              :y (* row-idx cell-edge-and-spacing-length)
                              :width cell-edge-length
                              :height cell-edge-length
                              :fill (if (alive? cell) "red" "black")
                              :onClick #(swap! state update-in [row-idx col-idx] toggle)}])
                row))
             @state)]]
          [:button {:on-click #(reset! state (gen-initial-board rows columns))}
           "Reset"]
          [:button {:on-click #(swap! state tick)}
           "Tick"]])))))

(defn ^:dev/after-load render []
  (dom/render
   [game 35 35]
   (.getElementById js/document "root")))

(defn -main []
  (render)
  (js/console.log "running"))
