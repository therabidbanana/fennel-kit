;;
;; Bundle file
;; Code changes will be overwritten
;;

;; title:  Disastrous Flying Critters
;; author: @therabidbanana
;; desc:   Help the Rainbow Witch Princess bring peace to the Kingdom
;; script: fennel

;; The base utils
;; [TQ-Bundler: kit.lib]

;; Utility functions to make fennel more like CLJ

(fn first [coll] (?. coll 1))
(fn last [coll] (?. coll (length coll)))
(fn nil? [x] (= x nil))

(fn ^in [x ...] (doto x (table.insert ...)))
(fn table? [x] (= (type x) :table))
(fn arr? [x] (and (table? x) (?. x 1)))

(fn cons [head rest]
  "Inserts head into rest, if array. Turns into array if not."
  (let [t? (table? rest)]
    (if
     (and (nil? head) t?)    rest
     (and (nil? rest) head)  [head]
     t?                      (^in rest 1 head)
     :else                   [head rest])))

(macro *args [val]
  `(if (arr? ,val) ,val (cons ,val [...])))

(macro hargs [val]
  `(if (and (not (arr? ,val)) (table? ,val))
       ,val
       (let [list# (cons ,val [...])
             acc# {}]
         (for [i# 1 (count list#) 2]
           (tset acc# (?. list# i#) (?. list# (+ i# 1))))
         acc#)))

(fn count [coll]
  (if (arr? coll)
      (length coll)
      (table? coll)
      (accumulate [count 0 i v (pairs coll)] (+ count 1))
      coll (length coll)
      nil))
(fn empty? [coll]
  (if (nil? coll) true (= 0 (count coll))))

(fn into [arr val ...]
  "Insert a list of values at the end of an existing list. "
  (assert (table? arr) "first arg must be collection")
  (let [coll (*args val)]
    (each [_ v (ipairs coll)]
      (^in arr v))
    arr))

(fn merge [h1 h2 ...]
  (let [new-hash
        (accumulate [new {}
                     k v (pairs h1)]
          (doto new (tset k v)))]
    (accumulate [new new-hash k v (pairs (hargs h2))] (doto new (tset k v)))))

(fn update [hash k func]
  (doto hash (tset k (func (?. hash k)))))

(fn min [a ...]
  "Find min of a collection (or list of arguments)"
  (let [coll (*args a)]
    (accumulate [min (first coll) _ v (ipairs coll)]
      (if (< v min) v min))))

(fn max [a ...]
  "Find max of a collection (or list of arguments)"
  (let [coll (*args a)]
    (accumulate [max (first coll) _ v (ipairs coll)]
      (if (< v max) max v))))

(fn sum [a ...]
  "Find sum of a collection (or list of arguments)"
  (let [coll (*args a)]
    (accumulate [acc 0 _ v (ipairs coll)]
      (if v (+ v acc) acc))))

(fn clamp [val min max]
  (if (> val max) max
      (< val min) min
      val))

(fn between? [val min max]
  (if (> val max) false
      (< val min) false
      true))

(fn mapv [func coll]
  (assert (table? coll) "last arg must be collection")
  (icollect [_ v (ipairs coll)] (func v)))

(fn filterv [func coll]
  (assert (table? coll) "last arg must be collection")
  (icollect [_ v (ipairs coll)]
    (if (func v) v)))

(fn mapiv [func coll]
  (assert (table? coll) "last arg must be collection")
  (icollect [i v (ipairs coll)] (func i v)))

(fn take [n coll]
  (assert (table? coll) "last arg must be collection")
  (accumulate [acc []
               i v (ipairs coll) :until (<= n (length acc))]
    (into acc v)))

(fn partition [arr max-cnt]
  "Takes an single dimension table and splits into pages of size count"
  (assert (table? arr) "first arg must be collection")
  (accumulate [acc [[]]
               index val (ipairs arr)]
    (if
     ;; If max <= current last size, new last
     (<= max-cnt (count (last acc)))
     (do (into acc [[val]]) acc)
     ;; Else, append to last
     (do (into (last acc) val) acc))))

(fn chars [str]
  (local acc [])
  (for [i 1 (count str)]
    (^in acc (string.sub str i i)))
  acc)

(fn words [str]
  (local acc [])
  (each [v (string.gmatch (string.gsub str "\n" " _NEWLINE_ ") "[^ \t]+")]
    (^in acc v))
  acc)


;; [/TQ-Bundler: kit.lib]

;; [TQ-Bundler: kit.ui.core]

;; [TQ-Bundler: kit.ui.utils]


;; ------ Text Helpers -----

(fn fit-lines [{: text : w : chr-size}]
  (let [chr-size (or chr-size 5) ;; too small to handle non-mono
        max-len (// w chr-size)]
    (var l 1)
    (local acc [""])
    (each [i v (ipairs (words text))]
      (if (= :_NEWLINE_ v) (do
                             (set l (+ 1 l))
                             (tset acc l ""))
          ;; Else
          (do
            (if (= nil (?. acc l))
                (tset acc l ""))
            ;; Space + fudge every seventh letter because char-size too small
            (if (>= (+ 1 (// (length (. acc l)) 7) (length (. acc l)) (length v)) max-len)
                (do
                  (set l (+ 1 l))
                  (tset acc l v))
                ;; else
                (if (= "" (or (?. acc l) ""))
                    (tset acc l v)
                    (tset acc l (.. (. acc l) " " v)))))))
    acc))

(fn text-box-lines [{: text : w : chr-size}]
  "Takes text and splits into lines that will fit in the specified width"
  (let [w (min w 240)
        chr-size (or chr-size 5)
        len (count text)]
    (fit-lines {: text : w})))

(fn pages-for-lines [{: lines : max-h : h : chr-size}]
  (let [chr-size (or chr-size 7)
        max-h (or max-h 68)
        h (or h (min (* chr-size (count lines)) max-h))
        line-count (max (// h chr-size) 1)]
    (partition lines line-count)))


;; [/TQ-Bundler: kit.ui.utils]

;; [TQ-Bundler: kit.ui.draw-utils]


(fn draw-box! [{: w : h : x : y : bg-color : border-color}]
  (if bg-color (rect x y w h bg-color))
  (if border-color (rectb x y w h border-color)))

(fn pick-animated-sprite [{ : w : animate : ticks : sprite}]
  (if (nil? animate)
      sprite
      (let [{ : period : steps } animate
            new-steps  (mapiv #(if (table? $2)
                                   (merge {:index $1} $2)
                                   {:index $1 :t $2})
                              steps)
            time-spot   (% ticks period)
            sheet-index (or (last (mapv #(if (>= time-spot $.t) $.index) new-steps)) 1)
            sprite-num  (* (or w 1) (- sheet-index 1))]
        (+ sprite-num sprite))))

(fn draw-sprite! [{: sprite : w : h : scale : trans : x : y
                   : animate : ticks
                   : flip : rotate
                   : anchor-x : anchor-y : shift-x : shift-y
                   : box}]
  (let [sprite (pick-animated-sprite { : ticks : w : sprite : animate})
        scale (or scale 1)
        w (or w 1)
        full-w (* 8 w scale)
        h (or h 1)
        full-h (* 8 h scale)
        x (if (= anchor-x :center) (- x (// full-w 2))
              (= anchor-x :right) (- x full-w)
              x)
        y (if (= anchor-y :center) (- y (// full-h 2))
              (= anchor-y :bottom) (- y full-h)
              y)
        x (+ x (or shift-x 0))
        y (+ y (or shift-y 0))]
    (if box (draw-box! (merge {:x x :y y :w full-w :h full-h} box)))
    (spr sprite x y (or trans -1) scale (or flip 0) (or rotate 0) w h)))

(fn draw-map! [{: w : h : x : y : sx : sy : trans : on-draw : scale}]
  (map (or x 0) (or y 0) (or w 30) (or h 17) (or sx 0) (or sy 0) (or trans -1) (or scale 1) on-draw))

(fn draw-right-arrow! [{: x : y : ticks}]
  (let [wobble (if (> (% (or ticks 0) 70) 40)
                   1
                   0)
        x (+ x wobble)]
    (tri x y
         (- x 4) (+ y 3)
         (- x 4) (- y 3)
         13)))

(fn draw-down-arrow! [{: x : y : ticks}]
  (let [wobble (if (> (% (or ticks 0) 70) 40)
                   1
                   0)
        y (- y wobble)]
    (tri x y
         (+ x 3) (- y 4)
         (- x 3) (- y 4)
         13)))


;; [/TQ-Bundler: kit.ui.draw-utils]


(global $ui {:components [] :t 0
             :defaults #(merge {:x 4 :y 80 :padding 5 :char-size 6
                                :bg-color 15 :border-color 12 :text-color 13}
                               (or $2 {}))
             :pop #(table.remove $.components)
             :clear-all! (fn [self tag]
                           (tset self :components (icollect [_ v (ipairs self.components)]
                                                    (if (and tag (not= tag v.tag)) v))))
             :push #(into $1.components $2)})

(fn box-styles [styles]
  (let [styles ($ui:defaults styles)
        pad2   (* styles.padding 2)
        w (- 236 styles.x)
        chars (if styles.chars
                  (+ (* styles.char-size styles.chars) pad2)
                  w)
        h (min 68 (- 135 styles.y))]
    (if (nil? styles.w) (tset styles :w (min w chars)))
    (if (nil? styles.h) (tset styles :h h))
    (merge styles {:inner-w (- styles.w pad2)
                   :inner-h (- styles.h pad2)})))

(macro defui [ui comp fns]
  (let [comp! (.. comp "!")]
    `(doto ,ui
       (tset ,comp (doto ,fns (tset :component ,comp)))
       (tset ,comp! (fn [self# params#] ((. (. ,ui ,comp) :open)
                                         (. ,ui ,comp)
                                         params#))))))

;; [TQ-Bundler: kit.ui.components.textbox]

(defui $ui
  :textbox
  {:open
   (fn [self {: box : text : tag : character : action}]
     (let [box (box-styles (or box {}))
           lines (text-box-lines {: text :w box.inner-w})
           pages (pages-for-lines {:max-h (max box.inner-h 12) : lines})
           page 1]
       ($ui:push (merge self {: action : character : tag : box : pages : page :ticks 0}))))
   :render
   (fn [{: box : page : pages : ticks : character}]
     (let [lines (?. pages page)
           more? (> (count pages) page)]
       (if lines
           (let [ticks (or ticks 1000)
                 u (+ box.x box.w -5)
                 v (+ box.y box.h -1)
                 letter-cnt (// ticks 2)
                 mid-x (+ box.x (// box.w 2))
                 mid-y (+ box.y (// box.h 2))
                 ]
             (if (and character (= character.position :left))
                 (draw-sprite! (merge character {:x box.x :y mid-y :anchor-x :right :anchor-y :center : ticks}))
                 character
                 (draw-sprite! (merge character {:x mid-x :y box.y :anchor-x :center :anchor-y :bottom : ticks})))
             (draw-box! box)
             (accumulate [prev-letters 0
                          ln line-text (ipairs lines)]
               (let [visible (- letter-cnt prev-letters)
                     tx (table.concat (take visible (chars line-text)) "")]
                 (print tx
                        (+ box.padding box.x)
                        (+ box.y box.padding -1 (* 8 (- ln 1)))
                        box.text-color)
                 (+ prev-letters (count line-text))))
             (if more? (draw-down-arrow! {:x u :y v : ticks }))))))
   :react
   (fn [self]
     (let [page (or self.page 1)
           lines (?. self.pages page)
           ticks (or self.ticks 0)]
       (if (btnp 4)
           (let [total-chars (sum (mapv count lines))
                 no-more (> (// ticks 2) total-chars)]
             (if no-more ;; unless typewriter effect still going
                 (do
                   (tset self :page (+ 1 page))
                   (tset self :ticks 0))
                 (do
                   (tset self :ticks (* total-chars 2))))))
       (tset self :ticks (+ (or self.ticks ticks) 1)))
     (if (> (or self.page 1) (or (count self.pages) 1))
         ;; Pop self
         (do
           ($ui:pop)
           (if self.action (self.action)))
         ))})


;; [/TQ-Bundler: kit.ui.components.textbox]

;; [TQ-Bundler: kit.ui.components.menu]


(defui $ui
  :menu
  {:open
   (fn [self {: box : options : tag}]
     (let [box (box-styles (merge (or box {}) {:chars 40}))
           selected 1]
       ($ui:push
        (merge self {: tag : box : options : selected :ticks 0}))))
   :render
   (fn [{: component : box : options : ticks : selected}]
     (let [lines (mapv #$.label options)]
       (let [ticks (or ticks 0)]
         (draw-box! box)
         (each [ln option (ipairs options)]
           (let [tx (?. option :label)
                 y-spot (+ box.y box.padding -1 (* 10 (- ln 1)))]
             (print tx (+ box.padding 4 box.x) y-spot box.text-color)
             (let [u (+ box.x 7)
                   v (+ y-spot 3)]
               (if (= selected ln)
                   (draw-right-arrow! {:x u :y v : ticks})))
             )))))
   :react
   (fn [self]
     (let [{ : selected : options} self
           dec-select  (max (- selected 1) 1)
           opt-count     (count options)
           inc-select  (min (+ 1 selected) opt-count)
           curr-option (?. options selected)
           action      (or curr-option.action #:no-action)
           keep-open?  curr-option.keep-open?
           ticks       (or self.ticks 0)]
       (if (btnp 0)
           (doto self
             (tset :ticks 0)
             (tset :selected dec-select))
           (btnp 1)
           (doto self
             (tset :ticks 0)
             (tset :selected inc-select))
           (btnp 4)
           (if keep-open?
               (action)
               (do ($ui:pop) (action))))
       (tset self :ticks (+ (or (?. self :ticks) ticks) 1))))})


;; [/TQ-Bundler: kit.ui.components.menu]

;; [TQ-Bundler: kit.ui.components.sprite-selector]

(defui $ui
  :sprite-selector
  {:open
   (fn [self {: box : map : sprites : tag}]
     ;; TODO - visit box styles?
     (let [box (box-styles (merge (or box {}) {:chars 40}))
           arrow :down
           selected 1]
       ($ui:push
        (merge self {: tag : box : map : sprites : selected :ticks 0 : arrow}))))
   :render
   (fn [{: component : box : map : sprites : ticks : selected : arrow}]
     (let [arrow-fn (if (= arrow :right) draw-right-arrow! draw-down-arrow!)]
       (let [ticks (or ticks 0)]
         ;; (draw-box! box)
         (if map (draw-map! map))
         (each [idx ent (ipairs sprites)]
           ;; (print tx (+ box.padding 4 box.x) y-spot box.text-color)
           (draw-sprite! ent)
           ;; TODO - handle arrow placement
           (let [u (+ ent.x (// (* (or ent.w 1) 8) 2))
                 v (- ent.y 4)]
             (if (= selected idx)
                 (arrow-fn {:x u :y v : ticks})))))))
   :react
   (fn [self]
     (let [{ : selected : sprites} self
           dec-select  (max (- selected 1) 1)
           opt-count     (count sprites)
           inc-select  (min (+ 1 selected) opt-count)
           curr-option (?. sprites selected)
           action      (or curr-option.action #:no-action)
           keep-open?  curr-option.keep-open?
           ticks       (or self.ticks 0)]
       (if (or (btnp 0) (btnp 2))
           (doto self
             (tset :ticks 0)
             (tset :selected dec-select))
           (or (btnp 1) (btnp 3))
           (doto self
             (tset :ticks 0)
             (tset :selected inc-select))
           (btnp 4)
           (if keep-open?
               (action)
               (do ($ui:pop) (action))))
       (tset self :ticks (+ (or (?. self :ticks) ticks) 1))))})


;; [/TQ-Bundler: kit.ui.components.sprite-selector]



;; Not clear if this should be true yet - render all components?
;; I think general idea is if a menu kicks a text box, menu still exists
(fn ui->display! []
  (each [i v (ipairs (or $ui.components []))]
    (v:render)))

(fn ui->active? [] (> (count $ui.components) 0))

(fn ui->react! []
  (let [v (last $ui.components)]
    (if (nil? v) v (v:react))))


;; [/TQ-Bundler: kit.ui.core]

;; [TQ-Bundler: kit.scene.core]

;; ---------------------
;; Scene Management
;; ---------------------

;; Entity

(fn react-entities! [self scene-state]
  (if (not (ui->active?))
      (let [scene-state (or scene-state {})
            entities (or self.entities [])
            reacted (mapv #(let [up ($:react (merge (or $.state {}) scene-state) self)]
                             (if (= :die up)
                                 nil
                                 (table? up)
                                 (doto $ (tset :state up))
                                 $))
                          entities)]
        (tset self :entities reacted))))

(fn draw-entities! [self scene-state]
  (let [scene-state (or scene-state {})
        entities (or self.entities [])]
    (mapv #($:render (merge (or $.state {}) scene-state) self) entities)))

(global $scene
        {:tick! #(let [scene-tick (. (or $.active {:tick #:noop}) :tick)
                       new-state   (scene-tick $.active $.active.state)]
                   (tset $.active :state new-state)
                   (ui->react!)
                   (ui->display!))
         :draw! #(let [scene-draw (. (or $.active {:draw #:noop}) :draw)]
                   (scene-draw $.active $.active.state)
                   (draw-entities! $.active $.active.state))
         :overdraw! #(let [scene-draw (. (or $.active {:overdraw #:noop}) :overdraw)]
                       (if scene-draw
                           (scene-draw $.active $.active.state))
                       (ui->display!))
         :active nil
         :scenes {}
         ;; Swap + prepare
         :select! (fn [self name ...] (let [scene (?. self.scenes name)]
                                    (tset self :active scene)
                                    (scene:prepare ...)))
         ;; Switch without preparing (allows pause scenes)
         :swap! (fn [self name] (let [scene (?. self.scenes name)]
                                    (tset self :active scene)))
         :add! (fn [self scene] (let [name scene.scene]
                                   (tset self.scenes name scene)))})

(macro defscene [scene name fns]
  `(let [scene-comp# (merge {:scene ,name} ,fns)]
     (tset (. ,scene :scenes) ,name scene-comp#)
     (: ,scene :add! scene-comp#)
     scene-comp#))


;; [/TQ-Bundler: kit.scene.core]


(fn inside? [{: x : y &as box} {:x x1 :y y1 &as point}]
  (and (>= x1 x) (<= x1 (+ x box.w))
       (>= y1 y) (<= y1 (+ y box.h))))

(fn touches? [{&as ent1} {&as ent2}]
  (and
   (< (+ ent1.x 0) (+ ent2.x ent2.w))
   (> (+ ent1.x ent1.w) (+ ent2.x 0))
   (< (+ ent1.y 0) (+ ent2.y ent2.h))
   (> (+ ent1.y ent1.h) (+ ent2.y 0))))

(fn collision-sides [{&as ent1} {&as ent2}]
  {:top (and (> ent1.y ent2.y)
             (< (+ ent1.y 0) (+ ent2.y ent2.h))
             (> (+ ent1.y ent1.h) (+ ent2.y 0)))
   :bottom (and (< ent1.y ent2.y)
                (< (+ ent1.y 0) (+ ent2.y ent2.h))
                (> (+ ent1.y ent1.h) (+ ent2.y 0)))
   :right (and (< ent1.x ent2.x)
                (< (+ ent1.x 0) (+ ent2.x ent2.w))
                (> (+ ent1.x ent1.w) (+ ent2.x 0)))
   :left (and (> ent1.x ent2.x)
              (< (+ ent1.x 0) (+ ent2.x ent2.w))
              (> (+ ent1.x ent1.w) (+ ent2.x 0)))})


;; -------

(var t 0)
(var player-sprite 256)
(var enemy-portal-colors {:red 32 :orange 40 :yellow 80 :green 88 :blue 128 :purple 136 :white 176})
(var enemy-portal-tiles {32 :red 40 :orange 80 :yellow 88 :green 128 :blue 136 :purple 176 :white})

(fn draw-entity [{ : character &as ent} state {: bounds &as game}]
  (let [shifted-x (- state.x (or state.screen-x 0))
        shifted-y (- state.y (or state.screen-y 0))]
    (draw-sprite! (merge (merge character state) {:x shifted-x
                                                  :y shifted-y}))))

;; TODO: Start figuring out tile coloration
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

(var tile-starts {:red 0     :orange 8
                  :yellow 48 :green 56
                  :blue 96   :purple 104
                  :grey 152})

(fn shift-tile-color [tile color]
  (let [col (% tile 16)
        row (// tile 16)
        current-color (tile-color tile)
        dist (- (?. tile-starts current-color) (?. tile-starts color))]
    (if (or (= current-color :none) (= color :none) (= tile 0))
        tile
        (< row 9)
        (- tile dist)
        tile)))

(fn bullet-react [{: color &as bullet} {: x : y : screen-x : screen-y &as state} {: entities &as game}]
  (let [collision   (bullet:collision-box)
        intersected (-?>> (filterv #(= :enemy $.tag) entities)
                          (filterv #(touches? collision ($:collision-box)))
                          (filterv #(not= $.color color))
                          first)
        x (+ x state.dx)
        y (+ y state.dy)
        {: would-paint?} (game:fetch-map-tile {: x : y : color})]
    (if intersected
        (do (intersected:take-damage! bullet) :die)
        (not (touches? {:x (+ -10 screen-x) :y (+ -10 screen-y) :w 260 :h 200} collision))
        :die
        would-paint?
        (do (game:paint-tile! {: x : y : color}) :die)
        (merge state {: x : y}))))

(var palette {:red 2 :orange 3 :yellow 4 :green 6 :blue 9 :purple 1})

(fn build-bullet [{: x : y : color : speed &as state}]
  (let [speed (or speed 2.5)
        dx    speed
        dy    0
        color (or color :red)]
    {:react bullet-react
     :state (merge state {: dx : dy : speed : color :h 2 :w 2})
     :collision-box (fn [{: state}] {:x state.x :y state.y :w 2 :h 2})
     :color color
     :render (fn [{&as bullet} {: x : y : color : h : w : screen-x : screen-y} _e]
               (let [shifted-x (- x screen-x)
                     shifted-y (- y screen-y)]
                 (rect shifted-x shifted-y w h (?. palette color))))}))


(var color-cycle [:red :orange :yellow :green :blue :purple])
(var next-color {:red :orange :orange :yellow :yellow :green :green :blue :blue :purple :purple :red})
(var prev-color {:red :purple :orange :red :yellow :orange :green :yellow :blue :green :purple :blue})

(fn player-collides? [tile]
  (and tile.solid? (or tile.would-paint? (= tile.color :grey))))

(fn player-react [{: firstp : secondp &as self}
                  {: x : y : dx : dy : color : dir : hp : invuln &as state}
                  {: entities : bounds &as game}]
  (let [max-speed 1.5
        drag 0.05
        add-speed 0.5
        gravity 0.05
        dx (or dx 0)
        dy (or dy 0)
        dx (if (btn (if firstp 2 secondp 10)) (max (- dx add-speed) (* -1 max-speed))
               (btn (if firstp 3 secondp 11)) (min (+ dx add-speed) max-speed)
               (< dx 0) (min (+ dx drag) 0)
               (max (- dx drag) 0)
               )
        dy (if (btn (if firstp 0 secondp 8)) (min -1 dy)
               (btn (if firstp 1 secondp 9)) (min (+ dy add-speed) max-speed)
               (< dy -2) (+ dy (* 4 gravity))
               (< dy 0.15) (+ dy gravity)
               (min (+ dy gravity) 0.15))
        x (+ x dx)
        y (+ y dy)
        dir (if (< dx -0.1) -1 (> dx 0.1) 1 (or dir 1))
        left? (= dir -1)
        x (clamp x (* bounds.x 8) (- (* 8 (+ bounds.x bounds.w)) 8)) ;; Limit to edges
        y (clamp y (- (* bounds.y 8) 8) (+ (* 8 (+ bounds.y bounds.h)) -12))
        collision   (self:collision-box)
        intersected (-?>> (filterv #(= :enemy $.tag) entities)
                          (filterv #(touches? collision ($:collision-box)))
                          first)
        invuln (or invuln 0)
        collisions (if intersected
                       (collision-sides (intersected:collision-box) collision))
        damaged? (and intersected (<= invuln 0) (not collisions.top))
        bounced? (and intersected collisions.top)
        hp (or hp 3)
        hp (if damaged? (- hp 1) hp)
        color (if (btnp (if firstp 6 secondp 14)) (?. prev-color color)
                  (btnp (if firstp 5 secondp 13)) (?. next-color color)
                  damaged? intersected.color
                  color)
        new-invuln (if damaged? 200 (max (- (or invuln 1) 1) 0))
        dy (if (and bounced? (btn (if firstp 1 secondp 9))) -4 bounced? -2.5 dy)
        y (if bounced? (+ y dy) y)
        ;; Handle bouncing
        foot-tile (game:fetch-map-tile {:x (+ x 7) :y (+ y 14) : color})
        dy (if (player-collides? foot-tile) (min dy 0) dy)
        y  (if (player-collides? foot-tile) (- foot-tile.y 14) y)
        head-tile (game:fetch-map-tile {:x (+ x 7) :y (+ y 2) : color})
        dy (if (player-collides? head-tile) (max dy 0.15) dy)
        y  (if (player-collides? head-tile) (+ head-tile.y 6) y)
        right-tile (game:fetch-map-tile {:x (+ x 14) :y (+ y 6) : color})
        dx (if (player-collides? right-tile) (min dx 0) dx)
        x  (if (player-collides? right-tile) (- right-tile.x 14) x)
        left-tile (game:fetch-map-tile {:x x :y (+ y 6) : color})
        dx (if (player-collides? left-tile) (max dx 0) dx)
        x  (if (player-collides? left-tile) (+ left-tile.x 8) x)
        ]
    (if bounced?
        (do
          (sfx 16 "E-5" 8 0 4)
          (intersected:take-damage! {: color : x : y})))
    (if damaged?
        (do
          (sfx 17 "G#7" 16 1 7))
        )
    (if (btnp (if firstp 4 secondp 12))
        ;; TODO: Is there a less sneaky way to add entity?
        (do 
          (sfx 18 "D-6" 8 0 4)
          (^in entities (build-bullet {:x (+ x (if left? 0 15)) :y (+ y 10) : color :speed (if left? -2 2)}))))
    (if (<= hp 0)
        :die
        (merge state {: x : y : dx : dy : color : dir :invuln new-invuln : hp}))))

(defscene $scene :pause
  {:tick
   (fn []
     (cls 14)
     (print "Paused..." 84 24 13))
   :prepare
   (fn []
     (poke 0x03FF8 8)
     ($ui:clear-all!)
     ($ui:menu! {:box {:x 50 :w 140}
                 :options [{:label "Play" :action #($scene:swap! :game)}
                           {:label "Quit" :action #($scene:select! :title)}]}))})

(var portraits {
                :princess {:position :left :sprite 201 :w 4 :h 4 :trans 0 :box {:bg-color 0 :border-color 13}}
                :advisor {:position :left :sprite 161 :w 4 :h 4 :trans 0 :box {:bg-color 0 :border-color 13}}
                })
(fn dialog-chain [after-action dialog ...]
  (let [next-dialogs [...]]
    (if (empty? next-dialogs)
       ($ui:textbox! (merge dialog :action after-action))
       ($ui:textbox! (merge dialog :action #(dialog-chain after-action (table.unpack next-dialogs)))))))


(defscene $scene :intro
  {:tick
   (fn []
     (cls 0)
     )
   :draw
   (fn []
     )
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
                    :character portraits.advisor
                    :text "Princess! Please help us! It's disastrous!"}
                   {:box {:x 34 :y 0}
                    :character portraits.princess
                    :text "What's happening?"}
                   {:box {:x 34}
                    :character portraits.advisor
                    :text "There's a bunch of critters, flying around the kingdoms, causing all sorts of destruction!"}
                   {:box {:x 34 :y 0}
                    :character portraits.princess
                    :text "Where did they come from?"}
                   {:box {:x 34}
                    :character portraits.advisor
                    :text "They appear to be crawling out of colorized portals!"}
                   {:box {:x 34 :y 0}
                    :character portraits.princess
                    :text "What!? I'll see what I can do with my Rainbow Witch powers!"}
                   {:box {:x 34}
                    :character portraits.advisor
                    :text "Be careful! They appear to be immune to weapons of their own color!"}
                   {:box {:x 34 :y 0}
                    :character portraits.princess
                    :text "Good thing I can change colors on demand!"}
                   )
     )})


(fn completion-rate [color current]
  (let [all-tiles (+ (sum (mapv #(or (?. current $) 0) color-cycle))
                     (or current.grey 0))
        all-tiles (max all-tiles 1) ;; Hack around possible div/0
        chosen    (or (?. current color) 0)]
    (// (* (/ chosen all-tiles) 100) 1)))

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
                                       color-cycle))
           total-completion-rate (// (* 100 (/ total-completion 600)) 1)]
       (poke 0x03FF8 0)
       ($ui:clear-all!)
       (dialog-chain #($scene:select! :title)
                     {:box {:x 34}
                      :character portraits.advisor
                      :text "Great work princess! Did all the monsters go away?"}
                     {:box {:x 34 :y 0}
                      :character portraits.princess
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
                                            :character portraits.princess
                                            :text (.. "Looks like I've already helped that world! - " (completion-rate color (?. completions color)) "%")
                                            }))
                           ;; else
                           (do
                             (set $scene.scenes.game.level color)
                             ($scene:select! :game))))
           sprite-pick  (fn -sprite-pick [color] (if (?. completions color)
                                                     enemy-portal-colors.white
                                                     (?. enemy-portal-colors color)))
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
           completed-count (-> (filterv #(?. completions $) color-cycle) count)]
       (if (< completed-count 6)
           (do
             (tset self :completions completions)
             (poke 0x03FF8 0)
             ($ui:clear-all!)
             ($ui:sprite-selector! map-details)
             (dialog-chain #:noop
                           {:box {:x 34 :y 0}
                            :character portraits.princess
                            :text "Where should I go?"
                            }))
           ;; Game end
           (do
             ($scene:select! :outro completions))))
     )})


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
     ;; (print "Disastrous Flying" 22 22 15 false 2)
     ;; (print "Critters" 68 52 15 false 2)
     ;; (print "Disastrous Flying" 23 23 13 false 2)
     ;; (print "Critters" 69 53 13 false 2)
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

(var sprite-colors {:red 256 :orange 264 :blue 320 :green 296 :purple 328 :yellow 288})

(fn build-player [base-state first-player]
  {:render (fn draw-player [{: character &as ent} {: dir : color : invuln &as state} _others]
             (let [sprite (if (> (% (or invuln 0) 33) 29)
                              (?. sprite-colors (?. next-color color))
                              (and (< (% (or invuln 0) 33) 9) (not= 0 invuln))
                              (?. sprite-colors (?. prev-color color))
                              (?. sprite-colors color))
                   flip (if (> (or dir 1) 0) 0 1)
                   shifted-x (- state.x state.screen-x)
                   shifted-y (- state.y state.screen-y)
                   ]
               (draw-sprite! (merge (merge character state)
                                    {:x shifted-x :y shifted-y : sprite : flip}))))
   :react player-react
   :collision-box (fn [{: state}] {:x (+ state.x 5) :y (+ state.y 4) :w 10 :h 10})
   :state (merge {:x 0 :y 0 :color :yellow}
                 base-state)
   :tag :player
   :firstp  first-player
   :secondp  (not first-player)
   :character
   {;; Test weird blink patterns
    :animate {:period 200 :steps [{:t 0 :index 1} {:t 100 :index 2} {:t 112 :index 1}
                                  {:t 115 :index 3} {:t 130 :index 1}]}
    :trans 0
    :w 2 :h 2}})

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

(var enemy-sprite-colors {:red 384 :orange 392 :yellow 416 :green 424 :blue 448 :purple 456})
(fn build-enemy [{: color &as base-state}]
  (let [color (or color :red)
        sprite (?. enemy-sprite-colors color)
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
        sprite (?. enemy-portal-colors color)]
    {:render (fn [self {: x : y : screen-x : screen-y : hp : max-hp &as state} {&as game}]
               (let [x (- x screen-x)
                     y (- y screen-y)]
                 (draw-box! {:x (+ x 2) :y (- y 4) :w 12 :h 3 :border-color (?. palette color)})
                 (draw-box! {:x (+ x 3) :y (- y 3) :w (* (/ hp max-hp) 10) :h 1 :bg-color (?. palette color)})
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

(fn build-home-portal [{: color : hp &as base-state}]
  (let [color (or color :white)
        sprite (?. enemy-portal-colors color)]
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
                    ;;                :character portraits.princess
                    ;;                :text "Time to head to the next problem!"
                    ;;                :action #($scene:select! :title)})
                    ($scene:select! :map level color-bar)
                    (> (or timer-ticks 0) (* 60 60))
                    ;; ($ui:textbox! {:box {:x 34}
                    ;;                :character portraits.princess
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

(fn draw-hud-colorbar [current]
  (let [all-tiles (+ (sum (mapv #(or (?. current $) 0) color-cycle))
                     (or current.grey 0))
        red-portion (* 234 (/ (or current.red 0) all-tiles))
        orange-portion (* 234 (/ (or current.orange 0) all-tiles))
        yellow-portion (* 234 (/ (or current.yellow 0) all-tiles))
        green-portion (* 234 (/ (or current.green 0) all-tiles))
        blue-portion (* 234 (/ (or current.blue 0) all-tiles))
        purple-portion (* 234 (/ (or current.purple 0) all-tiles))
        grey-portion (* 234 (/ (or current.grey 0) all-tiles))
        ]
    ;; (print (.. "All tiles: " all-tiles) 10 10 13)
    ;; (print (.. "Red tiles: " current.red) 10 30 13)
    (draw-box! {:x 3 :y 3 :w red-portion :h 2 :bg-color palette.red})
    (draw-box! {:x (sum 3 red-portion) :y 3 :w orange-portion :h 2 :bg-color palette.orange})
    (draw-box! {:x (sum 3 red-portion orange-portion) :y 3 :w yellow-portion :h 2 :bg-color palette.yellow})
    (draw-box! {:x (sum 3 red-portion orange-portion yellow-portion) :y 3 :w green-portion :h 2 :bg-color palette.green})
    (draw-box! {:x (sum 3 red-portion orange-portion yellow-portion green-portion) :y 3 :w blue-portion :h 2 :bg-color palette.blue})
    (draw-box! {:x (sum 3 red-portion orange-portion yellow-portion green-portion blue-portion) :y 3 :w purple-portion :h 2 :bg-color palette.purple})
    (draw-box! {:x (sum 3 red-portion orange-portion yellow-portion green-portion blue-portion purple-portion) :y 3 :w grey-portion :h 2 :bg-color 14})
    (draw-box! {:x 2 :y 2 :w 236 :h 4 :border-color 12})
    ))

(fn capitalize-word [str]
  (str:gsub "^%l" string.upper))

(fn draw-hud [{: level : entities &as game} { : screen-x : screen-y : color-bar : ticks}]
  (let [portals (filterv #(?. $ :portal) entities)
        critters (filterv #(?. $ :critter) entities)
        home    (-> (filterv #(= $.tag :home) entities) first)
        enemy-count (-> (filterv #(= :enemy $.tag) entities) count)]
    ;; (print (.. "screen-x: " screen-x) 10 20 13)
    (if (> enemy-count 0)
        (do
          (print (.. "Portals: " (count portals)) 4 10 13 false 1 true)
          (print (.. "Critters: " (count critters)) 180 10 13 false 1 true))
        home
        (do 
          (print (.. (capitalize-word level) " restored: " (completion-rate level color-bar) "%") 4 8 13 false 1 true)
          (print (.. "Time Remaining... " (- 60 (// (or home.state.timer-ticks 0) 60)) "s")
                 158 8 13 false 1 true))
        )
    )
  (draw-hud-colorbar color-bar))

(fn draw-sky! [{: ticks : screen-x}]
  )

(fn draw-stats [player first-player?]
  (if first-player?
      (do
        (print (.. "1P: " (or player.state.hp 3)) 11 121 15)
        (print (.. "1P: " (or player.state.hp 3)) 10 120 13)
        )
      (do
        (print (.. "2P: " (or player.state.hp 3)) 191 121 15)
        (print (.. "2P: " (or player.state.hp 3)) 190 120 13)
        )
      )
  ;; (print (.. "x:" (or first-player.state.x 0)) 10 110 13)
  ;; (print (.. "y:" (or first-player.state.y 0)) 10 100 13)
  )

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
              ($ui:textbox! {:box {:x 34} :text "Ouch!" :character portraits.princess}))
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
              ($ui:textbox! {:box {:x 34} :text "oof!" :character portraits.princess}))
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
          ($ui:textbox! {:box {:x 34} :text "Looks like I can head home. Maybe I should clean up a bit first?" :character portraits.princess})
          ;; Look through tile bounds
          (for [x bounds.x (- (+ bounds.x bounds.w) 1)]
            (for [y bounds.y (- (+ bounds.y bounds.h) 1)]
              (let [tile (mget x y)]
                (if
                 (= 240 tile)
                 (let [shifted-y (- y 6)]
                   (self:add-entity! (build-home-portal {:x (* x 8) :y (* shifted-y 8)})))
                 ))))))))

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


(defscene $scene :game
  {:state {}
   :tick
   (fn [{: bounds &as self} {: ticks : color-bar : screen-x : screen-y &as screen-state}]
     ;; (if (btnp 7) ($scene:select! :pause))
     (react-entities! self screen-state)
     (spawn-players! self true)
     (spawn-home-portal! self true)
     ;; (draw-sky! screen-state)
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
       ;; (icollect [_ v (ipairs self.entities)]
       ;;   (if (and (= :enemy v.tag) (> screen-state.ticks 60))
       ;;       (v:take-damage! {})))
       ;; (print (.. "Count " (count self.entities)) 20 20 13 )
       (cls 8) ;; Allow pretty sky
       {:ticks (+ screen-state.ticks 1) :screen-x new-screen-x : color-bar :screen-y new-screen-y}))
   :draw
   (fn [{: bounds &as self} {: screen-x : screen-y : color-bar &as screen-state}]
     (draw-sky! screen-state)
     (draw-map! {:x bounds.x :w bounds.w
                 :y bounds.y :h bounds.h
                 :sx (- 0 (- screen-x (* bounds.x 8))) :sy (- 0 (- screen-y (* bounds.y 8)))
                 :trans 0
                 :on-draw (fn [tile x y]
                            (if (between? tile 242 247)
                                (do (self:add-entity!
                                     (build-portal {
                                                    :color (?. color-cycle (- tile 241))
                                                    :dx -0.5 :x (* x 8) :y (* y 8) :hp 10}))
                                    (mset x y 0)
                                    0)
                                (?. enemy-portal-tiles tile)
                                (do (self:add-entity!
                                     (build-portal {
                                                    :color (?. enemy-portal-tiles tile)
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
   :entities []
   :add-entity! (fn [self ent] (into self.entities [ent]))
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
     (tset self :bounds (?. real-level-bounds self.level))
     (tset self :state {:ticks 0 :screen-x (* self.bounds.x 8) :color-bar {} :screen-y (* self.bounds.y 8)})
     ($ui:clear-all!)
     (self:recalculate-color-bar!)
     (spawn-players! self false)
     ;; (self:add-entity! (build-enemy {:dx -0.5 :x 200 :y 40 :hp 1}))
     ;; (self:add-entity! (build-enemy {:dx -0.5 :dy 1 :x 240 :y 100 :hp 1}))
     )})

(fn _G.BDR [line]
  (let [scans 288
        PALETTE_ADDR 0x03FC0
        CHANGE_COL 8
        color (* (// (* 0xff (/ line scans)) 8) 8)]
    (poke (+ (+ (* CHANGE_COL 3) 0) PALETTE_ADDR) color)
    (poke (+ (+ (* CHANGE_COL 3) 1) PALETTE_ADDR) color)
    (poke (+ (+ (* CHANGE_COL 3) 2) PALETTE_ADDR) color)

    ))

(fn _G.BOOT []
  ($scene:select! :title)
  )

(fn _G.TIC []
  ($scene:tick!)

  ($scene:draw!)
  )

(fn _G.OVR []
  ($scene:overdraw!)
  )
