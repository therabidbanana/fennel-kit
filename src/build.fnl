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

;; Shared config, can be updated by game
(global $config {:trace-timing false})

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

(fn inspect-serialize [val name skipnewlines depth]
  (let [skipnewlines (or skipnewlines false)
        depth (or depth 0)]
    (var tmp (string.rep " " depth))
    (when name (set tmp (.. tmp name " = ")))
    (if (= (type val) :table)
        (do
          (set tmp
               (.. tmp "{"
                   (or (and (not skipnewlines)
                            "\n")
                       "")))
          (each [k v (pairs val)]
            (set tmp
                 (.. tmp
                     (inspect-serialize v k
                                      skipnewlines
                                      (+ depth 1))
                     ","
                     (or (and (not skipnewlines)
                              "\n")
                         ""))))
          (set tmp
               (.. tmp (string.rep " " depth) "}")))
        (= (type val) :number)
        (set tmp (.. tmp (tostring val)))
        (= (type val) :string)
        (set tmp (.. tmp (string.format "%q" val)))
        (= (type val) :boolean)
        (set tmp
             (.. tmp (or (and val :true) :false)))
        (set tmp
             (.. tmp
                 "\"[datatype:" (type val) "]\"")))
    tmp))

(macro inspect [val name]
  (let [inspected (or name (tostring val))]
    `(let [result# ,val]
       (trace (inspect-serialize result# ,inspected))
       result#)))


;; [/TQ-Bundler: kit.lib]

;; [TQ-Bundler: kit.logic]

;; Game logic

;;; ------ String helpers

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

(fn capitalize-word [str]
  (str:gsub "^%l" string.upper))

;;; ------ Collision helpers

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



;; [/TQ-Bundler: kit.logic]

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

(fn draw-map! [{: w : h : x : y : sx : sy : trans : on-draw : scale : ticks : on-first-draw}]
  (let [draw-fn (if (and ticks (<= ticks 1))
                    on-first-draw
                    on-draw)]
    (map (or x 0) (or y 0)
         (or w 30) (or h 17)
         (or sx 0) (or sy 0) (or trans -1) (or scale 1) draw-fn)))

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
            reacted (mapv (fn do-entity-react! [$]
                            (let [up ($:react (merge (or $.state {}) scene-state) self)]
                              (if (= :die up)
                                  nil
                                  (table? up)
                                  (doto $ (tset :state up))
                                  $)))
                          entities)]
        (tset self :entities reacted))))

(fn draw-entities! [self scene-state]
  (let [scene-state (or scene-state {})
        entities (or self.entities [])]
    (mapv #($:render (merge (or $.state {}) scene-state) self) entities)))

(fn draw-entity [{ : character &as ent} state {: bounds &as game}]
  (let [shifted-x (- state.x (or state.screen-x 0))
        shifted-y (- state.y (or state.screen-y 0))]
    (draw-sprite! (merge (merge character state) {:x shifted-x
                                                  :y shifted-y}))))

(global $scene
        {:tick! (fn tick-scene [$]
                  (let [tick-start    (time)
                        active-screen (react-entities! $.active $.active.state)
                        entity-tick   (time)
                        _ ($:timing :react-entities! tick-start entity-tick)
                        new-state     (: $.active :tick $.active.state)
                        scene-tick    (time)
                        _ ($:timing :scene.tick entity-tick scene-tick)]
                    (tset $.active :state new-state)
                    (ui->react!)
                    ($:timing :tick! tick-start (time))))
         :draw! (fn draw-scene [$]
                  (let [draw-start (time)
                        scene-draw (. (or $.active {:draw #:noop}) :draw)
                        _ (scene-draw $.active $.active.state)
                        scene-time (time)]
                    ;; ($:timing :draw-scene draw-start scene-time)
                    (draw-entities! $.active $.active.state)
                    ;; ($:timing :draw-entities! scene-time (time))
                    ($:timing :draw! draw-start (time)))
                  )
         :overdraw! (fn overdraw-scene [$]
                      (let [scene-draw (. (or $.active {:overdraw #:noop}) :overdraw)]
                        (if scene-draw
                            (scene-draw $.active $.active.state))
                        (ui->display!)))
         :timing (fn trace-times [self tag start end]
                   (when $config.trace-timing
                     (tset self.timings tag
                           (take 10 (cons (- end start) (?. self.timings tag))))))
         :active nil
         :timings {}
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
  `(let [scene-comp# (merge {:scene ,name
                             :entities []
                             :add-entity! (fn [self# ent#] (into self#.entities [ent#]))
                             } ,fns)]
     (tset (. ,scene :scenes) ,name scene-comp#)
     (: ,scene :add! scene-comp#)
     scene-comp#))


;; [/TQ-Bundler: kit.scene.core]



;; -------


;; [TQ-Bundler: game.logic]



;; [/TQ-Bundler: game.logic]

;; [TQ-Bundler: game.config]

(set $config.trace-timing false)


;; [/TQ-Bundler: game.config]


;; [TQ-Bundler: game.scenes.title]

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


;; [/TQ-Bundler: game.scenes.title]

;; [TQ-Bundler: game.scenes.main]

;; [TQ-Bundler: game.entities.player]

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


;; [/TQ-Bundler: game.entities.player]


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


;; [/TQ-Bundler: game.scenes.main]


(fn _G.BOOT []
  ($scene:select! :title)
  )

(fn _G.TIC []
  (let [(ok? err) (pcall (. $scene :tick!) $scene)]
    (when (not ok?)
      (inspect $scene.active)
      (inspect err "Error")
      (error "Tick failed!")))
  (let [(ok? err) (pcall (. $scene :draw!) $scene)]
    (when (not ok?)
      (inspect $scene.active)
      (inspect err "Error")
      (error "Draw failed!"))))

(fn _G.OVR []
  ($scene:overdraw!)
  (when $config.trace-timing
    (let [timings {}
         test (collect [k v (pairs $scene.timings)]
                (tset timings k (/ (math.floor (* 1000 (/ (sum v) (count v)))) 1000)))]
     (print (inspect-serialize timings) 120 100)))
  )
