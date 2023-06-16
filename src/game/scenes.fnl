(include "kit.lib")
(include "kit.scene.core")
(include "game.config")

(defscene $scene :title
  {:state {}
   :tick
   (fn [self {&as screen-state}]
     {:ticks (+ (or screen-state.ticks 0) 1)})
   :draw
   (fn [self {: ticks &as screen-state}]
     (cls 0)
     (draw-sprite! {:sprite 16 :x 10 :y 4 :w 12 :h 4 :trans 0})
     (draw-sprite! {:sprite 80 :x 36 :y (+ 25 (* 5 (math.sin (% (/ ticks 60) 60)))) :w 12 :h 4 :trans 0})
     (draw-sprite! {:sprite 161 :x 60 :y 48 :w 10 :h 3 :trans 0})
     (draw-sprite! {:sprite 460 :x 150 :y 48 :w 4 :h 4 :trans 0})
     )
   :prepare
   (fn []
     (sync 0 1)
     (poke 0x03FF8 15)
     ($ui:clear-all!)
     ($ui:menu! {:box {:x 50 :w 140}
                 :options [{:label "Play Game" :action #(do (set $scene.scenes.game.two_mode false)
                                                            (set $scene.scenes.intro.two_mode false)
                                                            (set $scene.scenes.map.two_mode false)
                                                            ($scene:select! :intro))}
                           {:label "Play Two Player" :action #(do (set $scene.scenes.game.two_mode true)
                                                                  (set $scene.scenes.intro.two_mode true)
                                                                  (set $scene.scenes.map.two_mode true)
                                                                  ($scene:select! :intro))}]}))})


(fn dialog-chain [after-action dialog ...]
  (let [next-dialogs [...]]
    (if (empty? next-dialogs)
        ($ui:textbox! (merge dialog :action after-action))
        ($ui:textbox! (merge dialog :action #(dialog-chain after-action (table.unpack next-dialogs)))))))

(defscene $scene :intro
  {:tick
   (fn [] (cls 0))
   :draw
   (fn [])
   :prepare
   (fn []
     (sync 0)
     (poke 0x03FF8 0)
     ($ui:clear-all!)
     (dialog-chain #(do
                      ;; NEW GAME logic
                      (tset $scene.scenes.map :completions {})
                      ($scene:select! :map))
                   {:box {:x 34}
                    :character $config.portraits.advisor
                    :text "Princess! Please help us! It's disastrous!"}
                   {:box {:x 34 :y 0}
                    :character $config.portraits.princess
                    :text "What's happening?"}
                   {:box {:x 34}
                    :character $config.portraits.advisor
                    :text "There's a bunch of critters, flying around the kingdoms, causing all sorts of destruction!"}
                   {:box {:x 34 :y 0}
                    :character $config.portraits.princess
                    :text "Where did they come from?"}
                   {:box {:x 34}
                    :character $config.portraits.advisor
                    :text "They appear to be crawling out of colorized portals!"}
                   {:box {:x 34 :y 0}
                    :character $config.portraits.princess
                    :text "What!? I'll see what I can do with my Rainbow Witch powers!"}
                   {:box {:x 34}
                    :character $config.portraits.advisor
                    :text "Be careful! They appear to be immune to weapons of their own color!"}
                   {:box {:x 34 :y 0}
                    :character $config.portraits.princess
                    :text "Good thing I can change colors on demand!"}
                   )
     )})

(defscene $scene :outro
  {:tick
   (fn []
     (cls 0)
     )
   :draw
   (fn []
     )
   :prepare
   (fn [self completions]
     (let [total-completion (sum (mapv #(completion-rate $1
                                                         (or (?. completions $1) {}))
                                       $config.color-cycle))
           total-completion-rate (// (* 100 (/ total-completion 600)) 1)]
       (poke 0x03FF8 0)
       ($ui:clear-all!)
       (dialog-chain #($scene:select! :title)
                     {:box {:x 34}
                      :character $config.portraits.advisor
                      :text "Great work princess! Did all the monsters go away?"}
                     {:box {:x 34 :y 0}
                      :character $config.portraits.princess
                      :text "Yes! All the portals have been closed. Peace is restored to the kingdom."}
                     {:box {}
                      :text (.. "Completion Rate: " total-completion-rate "%")}))
     )})

(defscene $scene :map
  {:tick
   (fn []
     (cls 0)
     )
   :draw
   (fn []
     )
   :completions {}
   :prepare
   (fn [self level-name color-bar]
     (let [completions  self.completions
           completions  (if level-name
                            (merge self.completions {level-name color-bar})
                            completions)
           level-select (fn -level-select [color]
                          (if
                           (?. completions color)
                           (do
                             (dialog-chain #:noop
                                           {:box {:x 34 :y 0}
                                            :character $config.portraits.princess
                                            :text (.. "Looks like I've already helped that world! - " (completion-rate color (?. completions color)) "%")
                                            }))
                           ;; else
                           (do
                             (set $scene.scenes.game.level color)
                             ($scene:select! :game))))
           sprite-pick  (fn -sprite-pick [color] (if (?. completions color)
                                                     $config.enemy-portal-colors.white
                                                     (?. $config.enemy-portal-colors color)))
           map-details
           {
            :map {:x 210 :y 17 :trans 0}
            :sprites [{:h 1 :w 1 :sprite (sprite-pick :red) :trans 0 :x 32 :y 16
                       :keep-open? (?. completions :red)
                       :action #(level-select :red)}
                      {:h 1 :w 1 :sprite (sprite-pick :orange) :trans 0 :x 80 :y 40
                       :keep-open? (?. completions :orange)
                       :action #(level-select :orange)}
                      {:h 1 :w 1 :sprite (sprite-pick :yellow) :trans 0 :x 160 :y 16
                       :keep-open? (?. completions :yellow)
                       :action #(level-select :yellow)}
                      {:h 1 :w 1 :sprite (sprite-pick :green) :trans 0 :x 200 :y 72
                       :keep-open? (?. completions :green)
                       :action #(level-select :green)}
                      {:h 1 :w 1 :sprite (sprite-pick :blue) :trans 0 :x 104 :y 112
                       :keep-open? (?. completions :blue)
                       :action #(level-select :blue)}
                      {:h 1 :w 1 :sprite (sprite-pick :purple) :trans 0 :x 24 :y 72
                       :keep-open? (?. completions :purple)
                       :action #(level-select :purple)}
                      ]}
           completed-count (-> (filterv #(?. completions $) $config.color-cycle) count)]
       (if (< completed-count 6)
           (do
             (tset self :completions completions)
             (poke 0x03FF8 0)
             ($ui:clear-all!)
             ($ui:sprite-selector! map-details)
             (dialog-chain #:noop
                           {:box {:x 34 :y 0}
                            :character $config.portraits.princess
                            :text "Where should I go?"
                            }))
           ;; Game end
           (do
             ($scene:select! :outro completions))))
     )})




