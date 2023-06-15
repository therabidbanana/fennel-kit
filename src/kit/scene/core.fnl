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
        {:tick! #(let [scene-tick    (. (or $.active {:tick #:noop}) :tick)
                       active-screen (react-entities! $.active $.active.state)
                       new-state     (scene-tick $.active $.active.state)]
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
