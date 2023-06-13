(include "kit.lib")

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

