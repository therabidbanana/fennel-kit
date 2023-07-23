;;---- Game Functions -----
(fn completion-rate [color current]
  (let [all-tiles (+ (sum (mapv #(or (?. current $) 0) $config.color-cycle))
                     (or current.grey 0))
        all-tiles (max all-tiles 1) ;; Hack around possible div/0
        chosen    (or (?. current color) 0)]
    (// (* (/ chosen all-tiles) 100) 1)))

(fn tile-color [tile]
  (let [col (% tile 16)
        row (// tile 16)
        secondary? (>= col 8)]
    (if (>= row 12)
        :none
        (= tile 0)
        :none
        (< row 3)
        (if secondary? :orange :red)
        (< row 6)
        (if secondary? :green :yellow)
        (< row 9)
        (if secondary? :purple :blue)
        :else
        (if secondary? :grey :none)
        )))

(fn shift-tile-color [tile color]
  (let [col (% tile 16)
        row (// tile 16)
        current-color (tile-color tile)
        dist (- (?. $config.tile-starts current-color)
                (?. $config.tile-starts color))]
    (if (or (= current-color :none) (= color :none) (= tile 0))
        tile
        (< row 9)
        (- tile dist)
        tile)))
