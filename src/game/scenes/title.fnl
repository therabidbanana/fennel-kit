(defscene $scene :title
  {:state {}
   :tick
   (fn tick-title [self {&as screen-state}]
     {:ticks (+ (or screen-state.ticks 0) 1)})
   :draw
   (fn draw-title [self {: ticks &as screen-state}]
     (cls 0))
   :prepare
   (fn []
     (poke 0x03FF8 1)
     ($ui:clear-all!)
     ($ui:menu! {:box {:x 50 :w 140}
                 :options [{:label "Play Game" :action #($scene:select! :main)}]}))})
