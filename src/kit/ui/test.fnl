(include "kit.ui.draw-utils")

;; Testing functionality
(let [lines (text-box-lines {:text "This is a big page" :w 48 :max-h 12})]
  (assert (= 2 (count (pages-for-lines {:lines lines :max-h 14}))) "two pages of lines")
  (pages-for-lines {:lines lines :max-h 14}))
