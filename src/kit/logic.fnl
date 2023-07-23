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
