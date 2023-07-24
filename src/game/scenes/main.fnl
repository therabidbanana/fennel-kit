(include "game.entities.player")

(defscene $scene :main
  {:state {}
   :bounds {:x 0 :y 0 :w 30 :h 17}
   :tick
   (fn tick-main [self {&as screen-state}]
     {:ticks (+ (or screen-state.ticks 0) 1)})
   :draw
   (fn draw-main [self {: ticks &as screen-state}]
     (cls 0))
   :prepare
   (fn [self]
     (tset self.state :ticks 0)
     (poke 0x03FF8 1)
     (self:add-entity! (build-player))
     ($ui:clear-all!)
     )})
