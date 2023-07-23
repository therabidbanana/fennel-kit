(fn draw-hud-colorbar [current]
  (let [all-tiles (+ (sum (mapv #(or (?. current $) 0) $config.color-cycle))
                     (or current.grey 0))
        red-portion (* 234 (/ (or current.red 0) all-tiles))
        orange-portion (* 234 (/ (or current.orange 0) all-tiles))
        yellow-portion (* 234 (/ (or current.yellow 0) all-tiles))
        green-portion (* 234 (/ (or current.green 0) all-tiles))
        blue-portion (* 234 (/ (or current.blue 0) all-tiles))
        purple-portion (* 234 (/ (or current.purple 0) all-tiles))
        grey-portion (* 234 (/ (or current.grey 0) all-tiles))
        ]
    ;; (print (.. "All tiles: " all-tiles) 10 10 13)
    ;; (print (.. "Red tiles: " current.red) 10 30 13)
    (draw-box! {:x 3 :y 3 :w red-portion :h 2 :bg-color $config.palette.red})
    (draw-box! {:x (sum 3 red-portion) :y 3 :w orange-portion :h 2 :bg-color $config.palette.orange})
    (draw-box! {:x (sum 3 red-portion orange-portion) :y 3 :w yellow-portion :h 2 :bg-color $config.palette.yellow})
    (draw-box! {:x (sum 3 red-portion orange-portion yellow-portion) :y 3 :w green-portion :h 2 :bg-color $config.palette.green})
    (draw-box! {:x (sum 3 red-portion orange-portion yellow-portion green-portion) :y 3 :w blue-portion :h 2 :bg-color $config.palette.blue})
    (draw-box! {:x (sum 3 red-portion orange-portion yellow-portion green-portion blue-portion) :y 3 :w purple-portion :h 2 :bg-color $config.palette.purple})
    (draw-box! {:x (sum 3 red-portion orange-portion yellow-portion green-portion blue-portion purple-portion) :y 3 :w grey-portion :h 2 :bg-color 14})
    (draw-box! {:x 2 :y 2 :w 236 :h 4 :border-color 12})
    ))

(fn draw-hud [{: level : entities &as game} { : screen-x : screen-y : color-bar : ticks}]
  (let [portals (filterv #(?. $ :portal) entities)
        critters (filterv #(?. $ :critter) entities)
        home    (-> (filterv #(= $.tag :home) entities) first)
        enemy-count (-> (filterv #(= :enemy $.tag) entities) count)]
    ;; (print (.. "screen-x: " screen-x) 10 20 13)
    (if (> enemy-count 0)
        (do
          (print (.. "Portals: " (count portals)) 4 10 13 false 1 true)
          (print (.. "Critters: " (count critters)) 180 10 13 false 1 true))
        home
        (do 
          (print (.. (capitalize-word level) " restored: " (completion-rate level color-bar) "%") 4 8 13 false 1 true)
          (print (.. "Time Remaining... " (- 60 (// (or home.state.timer-ticks 0) 60)) "s")
                 158 8 13 false 1 true))
        )
    )
  (draw-hud-colorbar color-bar))

(fn draw-sky! [{: ticks : screen-x}]
  )

(fn draw-stats [player first-player?]
  (if first-player?
      (do
        (print (.. "1P: " (or player.state.hp 3)) 11 121 15)
        (print (.. "1P: " (or player.state.hp 3)) 10 120 13)
        )
      (do
        (print (.. "2P: " (or player.state.hp 3)) 191 121 15)
        (print (.. "2P: " (or player.state.hp 3)) 190 120 13)
        )
      )
  ;; (print (.. "x:" (or first-player.state.x 0)) 10 110 13)
  ;; (print (.. "y:" (or first-player.state.y 0)) 10 100 13)
  )
