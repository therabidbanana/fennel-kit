(fn enemy-react [{: color &as self}
                 {: hp : x : y : dx : dy : ticks : cycle &as state}
                 { : bounds &as game}]
  (let [left-bound (* (* bounds.x 8))
        right-bound (* (+ bounds.x bounds.w) 8)
        top-bound (+ (* bounds.y 8) 1)
        bottom-bound (* (+ bounds.y bounds.h) 8)
        x (clamp (if dx (+ x dx) x)
                 left-bound
                 (- right-bound 8))
        y (clamp (if dy (+ y dy) y)
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
               (math.sin (* (/ ticks cycle) 1)))
        {: would-paint? } (game:fetch-map-tile {:x (+ x 7) :y (+ y 7) : color})]
    (if (<= hp 0)
        :die
        (do
          (if would-paint? (game:paint-tile! {:x (+ x 7) :y (+ y 7) : color}))
          (merge state {: x : y : dx : dy})))))

(fn build-enemy [{: color &as base-state}]
  (let [color (or color :red)
        sprite (?. $config.enemy-sprite-colors color)
        cycle (+ 10 (* 50 (math.random)))]
    {:render draw-entity
     :react enemy-react
     :tag :enemy
     :critter true
     :color color
     :collision-box (fn [{: state}] {:x state.x :y state.y :w 16 :h 16})
     :state (merge {: color : cycle} (or base-state {}))
     :take-damage! (fn [self bullet]
                     (tset self.state :hp (- (or self.state.hp 1) 1)))
     :character
     {:sprite sprite :trans 0 :w 2 :h 2}}))
