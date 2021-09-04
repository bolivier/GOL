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

(defn alive? [cell] (= :alive cell))
(def dead? (complement alive?))
(defn get-neighbors [board [row col]]
  (for [drow  (range -1 2)
        dcol  (range -1 2)
        :when (not= 0 drow dcol)]
    (get-in board
            [(+ row drow) (+ col dcol)])))

(defn ->vec
  "converts a seq board into a vector board.
  `for` and other constructs represent things as seq's, not vectors"
  [board]
  (into [] (map (partial into [])) board))

(defn count-living-neighbors [board [row col]]
  (->> (get-neighbors board [row col])
       (filter alive?)
       count))

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

(defn tick-cell [board [row col]]
  (let [cell           (get-in board [row col])
        neighbor-count (count-living-neighbors board [row col])]
    (cond
      (and (alive? cell)
           (or (= 2 neighbor-count)
               (= 3 neighbor-count))) :alive

      (and (dead? cell)
           (= 3 neighbor-count)) :alive

      :else :dead)))

(defn tick [board]
  (->vec
   (map-indexed
    (fn [row-idx row]
      (map-indexed
       (fn [col-idx _]
         (tick-cell board [row-idx col-idx]))
       row))
    board)))

(defn game [rows columns]
  (let [state (r/atom (gen-initial-board rows columns))]
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
          "Tick"]]))))

(defn ^:dev/after-load render []
  (dom/render
   [game 35 35]
   (.getElementById js/document "root")))

(defn -main []
  (render)
  (js/console.log "running"))
