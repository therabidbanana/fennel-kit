(fn player-nearby? [{: x : y &as state} entities]
  (let [nearby-players
        (-?>> (filterv #(= :player $.tag) entities)
              (filterv #(< (math.abs (- $.state.x x)) 240))
              (filterv #(< (math.abs (- $.state.y y)) 144))
              count)]
    (> nearby-players 0)))

(fn portal-react [{: color &as self}
                  {: hp : cycle : x : y : dx : dy : ticks : stationary? &as state}
                  {: entities : bounds &as game}]
  (let [left-bound (* (* bounds.x 8))
        right-bound (* (+ bounds.x bounds.w) 8)
        top-bound (+ (* bounds.y 8) 1)
        bottom-bound (* (+ bounds.y bounds.h) 8)
        x (clamp (if dx (+ x dx) x)
                 left-bound
                 (- right-bound 8))
        y (clamp (if dy
                     (+ y dy)
                     y)
                 top-bound
                 (- bottom-bound 8))
        dx (if (< x (+ left-bound 1))
               (max (- 0 dx) 0.2)
               (> x (- right-bound 16))
               (min (- 0 dx) -0.2)
               dx)
        dy (if (< y (+ top-bound 1))
               (max (- 0 (or dy 1)) 0.2)
               (> y (- bottom-bound 16))
               (min (- 0 (or dy 1)) -0.2)
               stationary?
               (* -0.15 (math.sin (/ ticks 40)))
               (math.sin (/ ticks 40)))
        cycle (or cycle (+ 197 (// (* (math.random) 90) 1)))
        {: would-paint? } (game:fetch-map-tile {:x (+ x 7) :y (+ y 7) : color})
        player-is-nearby? (player-nearby? state entities)]
    (if (<= hp 0)
        :die
        (do
          (if would-paint? (game:paint-tile! {:x (+ x 7) :y (+ y 7) : color}))
          (if (and (= (% ticks cycle) 0) player-is-nearby?)
              (game:add-entity! (build-enemy {:color color
                                              :dx (if stationary?
                                                      (+ -0.5 (* -1 (+ (math.random) 0.05)))
                                                      (* -1 (+ (math.random) 0.05)))
                                              :x (+ x (- 10 (* 10 (math.random))))
                                              :y (+ y (- 10 (* 10 (math.random))))
                                              :hp 1})))
          (merge state {: x : y : dx : dy : cycle})))))

(fn build-portal [{: color : hp : stationary? &as base-state}]
  (let [color (or color :red)
        hp    (or hp 10)
        sprite (?. $config.enemy-portal-colors color)]
    {:render (fn [self {: x : y : screen-x : screen-y : hp : max-hp &as state} {&as game}]
               (let [x (- x screen-x)
                     y (- y screen-y)]
                 (draw-box! {:x (+ x 2) :y (- y 4) :w 12 :h 3 :border-color (?. $config.palette color)})
                 (draw-box! {:x (+ x 3) :y (- y 3) :w (* (/ hp max-hp) 10) :h 1 :bg-color (?. $config.palette color)})
                 (draw-entity self state game)))
     :react portal-react
     :tag :enemy
     :portal true
     :color color
     :collision-box (fn [{: state}] {:x state.x :y state.y :w 16 :h 16})
     :state (merge {: color :max-hp hp : hp} (or base-state {}))
     :take-damage! (fn [self bullet]
                     (tset self.state :hp (- (or self.state.hp 1) 1)))
     :character
     {:sprite sprite :trans 0 :w 1 :h 1 :scale 2}}))
