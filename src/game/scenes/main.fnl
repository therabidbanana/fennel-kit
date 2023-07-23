(fn mark-grey-tiles [{: level : entities : bounds : two_mode &as self} percentage]
  (for [x bounds.x (- (+ bounds.x bounds.w) 1)]
    (for [y bounds.y (- (+ bounds.y bounds.h) 1)]
      (let [tile (mget x y)]
        (if (> (* 100 (math.random)) (- 100 percentage))
            (self:paint-tile! {:color :grey :x (* x 8) :y (* y 8)}))))))

(fn spawn-players! [{: level : entities : bounds : two_mode &as self} during-game]
  (let [first-player (->> (filterv #(= $.firstp true) self.entities) first)]
    (if first-player
        :noop
        (do
          (if during-game
              ($ui:textbox! {:box {:x 34} :text "Ouch!" :character $config.portraits.princess}))
          (for [x bounds.x (- (+ bounds.x bounds.w) 1)]
            (for [y bounds.y (- (+ bounds.y bounds.h) 1)]
              (let [tile (mget x y)]
                (if
                 (= 240 tile)
                 (let [centered-x (- (* x 8) 120)]
                   (tset self.state :screen-x centered-x)
                   (tset self.state :screen-y (* bounds.y 8))
                   (self:add-entity! (build-player {:invuln (if during-game 200 0) :x (* x 8) :y (* y 8) :color level} true)))
                 (> (* 100 (math.random)) 95)
                 (let []
                   (if during-game (self:paint-tile! {:color :grey :x (* x 8) :y (* y 8)}))
                   ))))))))
  (let [first-player (->> (filterv #(= $.firstp true) self.entities) first)
        second-player (->> (filterv #(= $.secondp true) self.entities) first)]
    (if (or second-player (not two_mode))
        :noop
        (do
          (if during-game
              ($ui:textbox! {:box {:x 34} :text "oof!" :character $config.portraits.princess}))
          (let [x (+ first-player.state.x 8)
                y (+ first-player.state.y 8)]
            (self:add-entity! (build-player {:invuln (if during-game 200 0) :x x :y y :color level} false)))
          (if during-game (mark-grey-tiles self 5))))))

(fn spawn-home-portal! [{: entities : bounds &as self} during-game]
  (let [home-ent    (->> (filterv #(= :home $.tag) self.entities) first)
        enemy-count (->> (filterv #(= :enemy $.tag) self.entities) count)]
    (if (or home-ent (> enemy-count 0))
        :noop
        (do
          ($ui:textbox! {:box {:x 34} :text "Looks like I can head home. Maybe I should clean up a bit first?" :character $config.portraits.princess})
          ;; Look through tile bounds
          (for [x bounds.x (- (+ bounds.x bounds.w) 1)]
            (for [y bounds.y (- (+ bounds.y bounds.h) 1)]
              (let [tile (mget x y)]
                (if
                 (= 240 tile)
                 (let [shifted-y (- y 6)]
                   (self:add-entity! (build-home-portal {:x (* x 8) :y (* shifted-y 8)})))
                 ))))))))

(defscene $scene :game
  {:state {}
   :tick
   (fn [{: bounds &as self}
        {: ticks : color-bar : screen-x : screen-y &as screen-state}]
     (spawn-players! self true)
     (spawn-home-portal! self true)
     (let [player-ent (->> (filterv #(= $.firstp true) self.entities) first)
           player-offset-x (- player-ent.state.x screen-x)
           diffx (- (clamp player-offset-x 80 160) player-offset-x)
           shifted-x (- screen-state.screen-x diffx)
           new-screen-x (clamp shifted-x (* 8 bounds.x) (- (* 8 (+ bounds.x bounds.w)) 240))
           player-offset-y (- player-ent.state.y screen-y)
           diffy (- (clamp player-offset-y 34 94) player-offset-y)
           shifted-y (- screen-state.screen-y diffy)
           new-screen-y (clamp shifted-y (* 8 bounds.y) (* 8 (+ bounds.y bounds.h -17)))
           second-player (->> (filterv #(= $.secondp true) self.entities) first)]
       ;; second player dragging along logic
       (if (and second-player (or (< second-player.state.x (- new-screen-x 8))
                                  (> second-player.state.x (+ new-screen-x 220))))
           (tset second-player.state :x
                 (clamp (- second-player.state.x diffx)
                        (* 8 bounds.x)
                        (* 8 (+ bounds.x bounds.w)))
                 ))
       (if (and second-player (or (< second-player.state.y (- new-screen-y 8))
                                  (> second-player.state.y (+ new-screen-y 128))))
           (tset second-player.state :y
                 (clamp (- second-player.state.y diffy)
                        (* 8 bounds.y)
                        (* 8 (+ bounds.y bounds.h)))))
       (if (= (% ticks 60) 0)
           (self:recalculate-color-bar!))
       (if (empty? self.entities)
           ($scene:select! :title))
       (cls 8) ;; Allow pretty sky
       {:ticks (+ screen-state.ticks 1) :screen-x new-screen-x : color-bar :screen-y new-screen-y}))
   :draw
   (fn [{: bounds &as self} {: ticks : screen-x : screen-y : color-bar &as screen-state}]
     (draw-sky! screen-state)
     (draw-map! {:x bounds.x :w bounds.w
                 :y bounds.y :h bounds.h
                 :sx (- 0 (- screen-x (* bounds.x 8))) :sy (- 0 (- screen-y (* bounds.y 8)))
                 :trans 0
                 :ticks ticks
                 :on-first-draw
                 (fn map-first-draw [tile x y]
                   (if
                    (between? tile 242 247)
                    (do (self:add-entity!
                         (build-portal {
                                        :color (?. $config.color-cycle (- tile 241))
                                        :dx -0.5 :x (* x 8) :y (* y 8) :hp 10}))
                        (mset x y 0)
                        0)
                    (?. $config.enemy-portal-tiles tile)
                    (do (self:add-entity!
                         (build-portal {
                                        :color (?. $config.enemy-portal-tiles tile)
                                        :dx 0 :x (* x 8) :y (* y 8) :hp 10
                                        :stationary? true
                                        :cycle 97
                                        }))
                        (mset x y 0)
                        0)
                    (= tile 240)
                    (do (set self.state.home-x (* x 8))
                        (set self.state.home-y (* y 8))
                        tile)
                    tile)
                   )
                 }))
   :overdraw
   (fn [self screen-state]
     (let [player-ent  (->> (filterv #(= $.firstp true) self.entities) first)
           player2-ent (->> (filterv #(= $.secondp true) self.entities) first)]
       (if player-ent (draw-stats player-ent true))
       (if player2-ent (draw-stats player2-ent false))
       (draw-hud self screen-state)
       ))
   :fetch-map-tile (fn fetch-map-tile [{: state : bounds &as self} {: x : y : color}]
                     (let [tile-x (// (+ x 0) 8)
                           tile-y (// (+ y 0) 8)
                           tile (mget tile-x tile-y)
                           colorable? (between? tile 1 144)
                           tile-color (tile-color tile)
                           solid? (fget tile 0)
                           oob? (or (< tile-x bounds.x) (> tile-x (+ bounds.x bounds.w))
                                    (< tile-y bounds.y) (> tile-y (+ bounds.y bounds.h)))
                           would-paint? (and
                                         (not= tile-color :grey)
                                         (not= tile-color :none)
                                         (not= color tile-color)
                                         (not oob?))]
                       {:x (* 8 (- tile-x 0))
                        :y (* 8 (- tile-y 0))
                        : tile-x : tile-y : oob? : solid? : tile : colorable? :color tile-color : would-paint?}))
   :paint-tile! (fn [{: state &as self} {: x :  y : color &as input}]
                  (let [{: tile-x : tile-y
                         :color tile-color : would-paint?
                         : tile : colorable? } (self:fetch-map-tile input)
                        in-color (or (?. state.color-bar color) 0)
                        out-color (or (?. state.color-bar tile-color) 0)]
                    (if (or would-paint? (and colorable? (= color :grey)))
                        (do
                          (doto state.color-bar
                            (tset color (+ in-color 1))
                            (tset tile-color (max (- out-color 1) 0)))
                          (mset tile-x tile-y (shift-tile-color tile color))))))
   :recalculate-color-bar!
   (fn [{: state : bounds : level &as self}]
     (let [{: map-y} state
           color-bar {:grey 0}]
       (for [x bounds.x (- (+ bounds.x bounds.w) 1)]
         (for [y bounds.y (- (+ bounds.y bounds.h) 1)]
           (let [t-color (tile-color (mget x y))
                 curr-val (or (?. color-bar t-color) 0)]
             (if (not= t-color :none)
                 (tset color-bar
                       t-color
                       (+ curr-val 1))))))
       (tset state :color-bar color-bar)))
   :prepare
   (fn prepare-game [self]
     (poke 0x03FF8 0)
     (tset self :entities [])
     (tset self :bounds (?. $config.level-bounds self.level))
     (tset self :state {:ticks 0 :screen-x (* self.bounds.x 8) :color-bar {} :screen-y (* self.bounds.y 8)})
     ($ui:clear-all!)
     (self:recalculate-color-bar!)
     (spawn-players! self false)
     ;; (self:add-entity! (build-enemy {:dx -0.5 :x 200 :y 40 :hp 1}))
     ;; (self:add-entity! (build-enemy {:dx -0.5 :dy 1 :x 240 :y 100 :hp 1}))
     )})
