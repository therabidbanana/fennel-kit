(include "kit.ui.utils")
(include "kit.ui.draw-utils")

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

;; Not clear if this should be true yet - render all components?
;; I think general idea is if a menu kicks a text box, menu still exists
(fn ui->display! []
  (each [i v (ipairs (or $ui.components []))]
    (v:render)))

(fn ui->active? [] (> (count $ui.components) 0))

(fn ui->react! []
  (let [v (last $ui.components)]
    (if (nil? v) v (v:react))))
