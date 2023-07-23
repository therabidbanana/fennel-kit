;; title:  Disastrous Flying Critters
;; author: @therabidbanana
;; desc:   Help the Rainbow Witch Princess bring peace to the Kingdom
;; script: fennel

;; The base utils
(include "kit.lib")
(include "kit.logic")
(include "kit.ui.core")
(include "kit.scene.core")


;; -------


(include "game.logic")
(include "game.config")

(include "game.entities.player")
(include "game.entities.enemy")
(include "game.entities.portal")
(include "game.entities.home-portal")

(include "game.draw-hud")

(include "game.scenes.other")
(include "game.scenes.main")

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
  (let [(ok? err) (pcall (. $scene :tick!) $scene)]
    (when (not ok?)
      (inspect $scene.active)
      (inspect err "Error")
      (error "Tick failed!"))))

(fn _G.OVR []
  ($scene:draw!) ;; here to avoid bdr
  ($scene:overdraw!)
  (when $config.trace-timing
    (let [timings {}
         test (collect [k v (pairs $scene.timings)]
                (tset timings k (/ (math.floor (* 1000 (/ (sum v) (count v)))) 1000)))]
     (print (inspect-serialize timings) 120 100)))
  )
