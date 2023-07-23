(fn build-home-portal [{: color : hp &as base-state}]
  (let [color (or color :white)
        sprite (?. $config.enemy-portal-colors color)]
    {:render draw-entity
     :react (fn [{: color &as self}
                 {: hp : timer-ticks : cycle : x : y : dx : dy :  ticks : color-bar &as state}
                 {: entities : level &as game}]
              (let [dy (* 0.15 (math.sin (/ ticks 40)))
                    player-ent (->> (filterv #(= :player $.tag) entities)
                                    (filterv #(= $.firstp true))
                                    first)]
                (if (touches? (self:collision-box) (player-ent:collision-box))
                    ;; ($ui:textbox! {:box {:x 34}
                    ;;                :character $config.portraits.princess
                    ;;                :text "Time to head to the next problem!"
                    ;;                :action #($scene:select! :title)})
                    ($scene:select! :map level color-bar)
                    (> (or timer-ticks 0) (* 60 60))
                    ;; ($ui:textbox! {:box {:x 34}
                    ;;                :character $config.portraits.princess
                    ;;                :text "Time to head to the next problem!"
                    ;;                :action #($scene:select! :title)})
                    ($scene:select! :map level color-bar)
                    :else
                    (merge state {:timer-ticks (+ (or timer-ticks 0) 1) :y (+ y dy) :dy dy}))))
     :tag :home
     :portal true
     :color :white
     :collision-box (fn [{: state}] {:x state.x :y state.y :w 16 :h 16})
     :state (merge {: color :max-hp hp : hp} (or base-state {}))
     :take-damage! (fn [self bullet] :noop)
     :character
     {:sprite sprite :trans 0 :w 1 :h 1 :scale 2}}))
