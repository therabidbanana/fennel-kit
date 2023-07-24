(fn player-react [{: firstp : secondp &as self}
                  {: x : y : dx : dy : color : dir : hp : invuln &as state}
                  {: entities : bounds &as game}]
  (let [max-speed 1.5
        drag 1
        add-speed 1
        gravity 0
        dx (or dx 0)
        dy (or dy 0)
        dx (if (btn 2) (max (- dx add-speed) (* -1 max-speed))
               (btn 3) (min (+ dx add-speed) max-speed)
               (< dx 0) (min (+ dx drag) 0)
               (max (- dx drag) 0)
               )
        dy (if (btn 0) -1
               (btn 1) 1
               0)
        x (+ x dx)
        y (+ y dy)
        dir (if (< dx -0.1) -1 (> dx 0.1) 1 (or dir 1))
        left? (= dir -1)
        x (clamp x (* bounds.x 8) (- (* 8 (+ bounds.x bounds.w)) 8)) ;; Limit to edges
        y (clamp y (- (* bounds.y 8) 8) (+ (* 8 (+ bounds.y bounds.h)) -12))
        ]
    (if (btnp 4)
        ;; TODO: Is there a less sneaky way to add entity?
        (do 
          (sfx 18 "D-6" 8 0 4)))
    (merge state {: x : y : dx : dy : dir })))

(var walking-animation {:period 30 :steps [{:t 0 :index 1}
                                            {:t 10 :index 2}
                                            {:t 20 :index 3}]})
(var standing-animation {:period 200 :steps [{:t 0 :index 1}]})

(fn build-player [base-state first-player]
  {:render
   (fn draw-player [{: character &as ent} {: dx : dir &as state} _others]
     (let [sprite 0
           flip (if (> 0 (or dir 1)) 0 1)
           animate (if (> (math.abs (or dx 0)) 0) walking-animation standing-animation)]
       (draw-sprite! (merge (merge character state)
                            {:x shifted-x :y shifted-y : sprite : flip : animate}))))
   :react player-react
   :collision-box (fn [{: state}] {:x (+ state.x 5) :y (+ state.y 4) :w 10 :h 10})
   :state (merge {:x 0 :y 0} base-state)
   :tag :player
   :character
   {;; Test weird blink patterns
    
    :trans 0
    :w 2 :h 2}})
