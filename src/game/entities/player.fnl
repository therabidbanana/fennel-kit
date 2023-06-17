(fn bullet-react [{: color &as bullet} {: x : y : screen-x : screen-y &as state} {: entities &as game}]
  (let [collision   (bullet:collision-box)
        intersected (-?>> (filterv #(= :enemy $.tag) entities)
                          (filterv #(touches? collision ($:collision-box)))
                          (filterv #(not= $.color color))
                          first)
        x (+ x state.dx)
        y (+ y state.dy)
        {: would-paint?} (game:fetch-map-tile {: x : y : color})]
    (if intersected
        (do (intersected:take-damage! bullet) :die)
        (not (touches? {:x (+ -10 screen-x) :y (+ -10 screen-y) :w 260 :h 200} collision))
        :die
        would-paint?
        (do (game:paint-tile! {: x : y : color}) :die)
        (merge state {: x : y}))))

(fn build-bullet [{: x : y : color : speed &as state}]
  (let [speed (or speed 2.5)
        dx    speed
        dy    0
        color (or color :red)]
    {:react bullet-react
     :state (merge state {: dx : dy : speed : color :h 2 :w 2})
     :collision-box (fn [{: state}] {:x state.x :y state.y :w 2 :h 2})
     :color color
     :render (fn [{&as bullet} {: x : y : color : h : w : screen-x : screen-y} _e]
               (let [shifted-x (- x screen-x)
                     shifted-y (- y screen-y)]
                 (rect shifted-x shifted-y w h (?. $config.palette color))))}))


(fn player-collides? [tile]
  (and tile.solid? (or tile.would-paint? (= tile.color :grey))))

(fn player-react [{: firstp : secondp &as self}
                  {: x : y : dx : dy : color : dir : hp : invuln &as state}
                  {: entities : bounds &as game}]
  (let [max-speed 1.5
        drag 0.05
        add-speed 0.5
        gravity 0.05
        dx (or dx 0)
        dy (or dy 0)
        dx (if (btn (if firstp 2 secondp 10)) (max (- dx add-speed) (* -1 max-speed))
               (btn (if firstp 3 secondp 11)) (min (+ dx add-speed) max-speed)
               (< dx 0) (min (+ dx drag) 0)
               (max (- dx drag) 0)
               )
        dy (if (btn (if firstp 0 secondp 8)) (min -1 dy)
               (btn (if firstp 1 secondp 9)) (min (+ dy add-speed) max-speed)
               (< dy -2) (+ dy (* 4 gravity))
               (< dy 0.15) (+ dy gravity)
               (min (+ dy gravity) 0.15))
        x (+ x dx)
        y (+ y dy)
        dir (if (< dx -0.1) -1 (> dx 0.1) 1 (or dir 1))
        left? (= dir -1)
        x (clamp x (* bounds.x 8) (- (* 8 (+ bounds.x bounds.w)) 8)) ;; Limit to edges
        y (clamp y (- (* bounds.y 8) 8) (+ (* 8 (+ bounds.y bounds.h)) -12))
        collision   (self:collision-box)
        intersected (-?>> (filterv #(= :enemy $.tag) entities)
                          (filterv #(touches? collision ($:collision-box)))
                          first)
        invuln (or invuln 0)
        collisions (if intersected
                       (collision-sides (intersected:collision-box) collision))
        damaged? (and intersected (<= invuln 0) (not collisions.top))
        bounced? (and intersected collisions.top)
        hp (or hp 3)
        hp (if damaged? (- hp 1) hp)
        color (if (btnp (if firstp 6 secondp 14)) (?. $config.prev-color color)
                  (btnp (if firstp 5 secondp 13)) (?. $config.next-color color)
                  damaged? intersected.color
                  color)
        new-invuln (if damaged? 200 (max (- (or invuln 1) 1) 0))
        dy (if (and bounced? (btn (if firstp 1 secondp 9))) -4 bounced? -2.5 dy)
        y (if bounced? (+ y dy) y)
        ;; Handle bouncing
        foot-tile (game:fetch-map-tile {:x (+ x 7) :y (+ y 14) : color})
        dy (if (player-collides? foot-tile) (min dy 0) dy)
        y  (if (player-collides? foot-tile) (- foot-tile.y 14) y)
        head-tile (game:fetch-map-tile {:x (+ x 7) :y (+ y 2) : color})
        dy (if (player-collides? head-tile) (max dy 0.15) dy)
        y  (if (player-collides? head-tile) (+ head-tile.y 6) y)
        right-tile (game:fetch-map-tile {:x (+ x 14) :y (+ y 6) : color})
        dx (if (player-collides? right-tile) (min dx 0) dx)
        x  (if (player-collides? right-tile) (- right-tile.x 14) x)
        left-tile (game:fetch-map-tile {:x x :y (+ y 6) : color})
        dx (if (player-collides? left-tile) (max dx 0) dx)
        x  (if (player-collides? left-tile) (+ left-tile.x 8) x)
        ]
    (if bounced?
        (do
          (sfx 16 "E-5" 8 0 4)
          (intersected:take-damage! {: color : x : y})))
    (if damaged?
        (do
          (sfx 17 "G#7" 16 1 7))
        )
    (if (btnp (if firstp 4 secondp 12))
        ;; TODO: Is there a less sneaky way to add entity?
        (do 
          (sfx 18 "D-6" 8 0 4)
          (^in entities (build-bullet {:x (+ x (if left? 0 15)) :y (+ y 10) : color :speed (if left? -2 2)}))))
    (if (<= hp 0)
        :die
        (merge state {: x : y : dx : dy : color : dir :invuln new-invuln : hp}))))


(fn build-player [base-state first-player]
  {:render (fn draw-player [{: character &as ent} {: dir : color : invuln &as state} _others]
             (let [sprite (if (> (% (or invuln 0) 33) 29)
                              (?. $config.player-sprite-colors (?. $config.next-color color))
                              (and (< (% (or invuln 0) 33) 9) (not= 0 invuln))
                              (?. $config.player-sprite-colors (?. $config.prev-color color))
                              (?. $config.player-sprite-colors color))
                   flip (if (> (or dir 1) 0) 0 1)
                   shifted-x (- state.x state.screen-x)
                   shifted-y (- state.y state.screen-y)
                   ]
               (draw-sprite! (merge (merge character state)
                                    {:x shifted-x :y shifted-y : sprite : flip}))))
   :react player-react
   :collision-box (fn [{: state}] {:x (+ state.x 5) :y (+ state.y 4) :w 10 :h 10})
   :state (merge {:x 0 :y 0 :color :yellow}
                 base-state)
   :tag :player
   :firstp  first-player
   :secondp  (not first-player)
   :character
   {;; Test weird blink patterns
    :animate {:period 200 :steps [{:t 0 :index 1} {:t 100 :index 2} {:t 112 :index 1}
                                  {:t 115 :index 3} {:t 130 :index 1}]}
    :trans 0
    :w 2 :h 2}})
