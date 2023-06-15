(var $config
     {
      :portraits
      {:princess {:position :left :sprite 201 :w 4 :h 4
                  :trans 0 :box {:bg-color 0 :border-color 13}}
       :advisor {:position :left :sprite 161 :w 4 :h 4
                 :trans 0 :box {:bg-color 0 :border-color 13}}}
      })

(var palette {:red 2 :orange 3 :yellow 4 :green 6 :blue 9 :purple 1})
(var color-cycle [:red :orange :yellow :green :blue :purple])
(var next-color {:red :orange :orange :yellow :yellow :green :green :blue :blue :purple :purple :red})
(var prev-color {:red :purple :orange :red :yellow :orange :green :yellow :blue :green :purple :blue})

(var tile-starts {:red 0     :orange 8
                  :yellow 48 :green 56
                  :blue 96   :purple 104
                  :grey 152})

(var t 0)
(var player-sprite 256)
(var enemy-portal-colors {:red 32 :orange 40 :yellow 80 :green 88 :blue 128 :purple 136 :white 176})
(var enemy-portal-tiles {32 :red 40 :orange 80 :yellow 88 :green 128 :blue 136 :purple 176 :white})

(var sprite-colors {:red 256 :orange 264 :blue 320 :green 296 :purple 328 :yellow 288})

(var enemy-sprite-colors {:red 384 :orange 392 :yellow 416 :green 424 :blue 448 :purple 456})


(var test-level-bounds
     {:yellow {:x 0 :y 0
               :w (* 30 5) :h 17}
      :red {:x 0 :y 0
            :w (* 30 5) :h 17}
      :orange {:x 0 :y 0
               :w (* 30 5) :h 17}
      :blue {:x 0 :y 17
             :w (* 30 3) :h 20}
      :purple {:x 0 :y 17
               :w (* 30 3) :h 17}
      :green {:x 0 :y 17
              :w (* 30 3) :h 17}
      })

(var real-level-bounds
     {:yellow {:x (* 30 5) :y (* 17 4)
               :w (* 30 2) :h (* 17 4)}
      :red {:x 0 :y (* 17 4)
            :w (* 30 5) :h 17}
      :orange {:x 0 :y 0
               :w (* 30 5) :h 17}
      :blue {:x 0 :y (* 17 3)
             :w (* 30 3) :h 17}
      :purple {:x 0 :y (* 17 2)
               :w (* 30 6) :h 17}
      :green {:x 0 :y (* 17 5)
              :w (* 30 4) :h (* 17 2)}
      })
