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

(include "game.scenes.title")
(include "game.scenes.main")

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
