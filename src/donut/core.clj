(ns donut.core
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str])
  (:gen-class))

(def conf
  {
   :r1 1
   :r2 2
   ; :k1 1
   ; :k1 30
   :k2 5
   :theta-spacing 0.07
   :phi-spacing 0.03

   :cols (Long/parseLong (str/trim (:out (sh/sh "tput" "cols"))))
   :rows (Long/parseLong (str/trim (:out (sh/sh "tput" "lines"))))
   :term-reset-str (:out (sh/sh "tput" "clear"))
   :luminance-chars ["." "," "-" "~" ":" ";" "=" "!" "*" "#" "$" "@"]
   })

(comment
  (int (/ (* (:cols conf) (:k2 conf) 3)
      (* 8 (+ (:r1 conf) (:r2 conf)))))
  )

(defn term-reset []
  (print (:term-reset-str conf)))

(defn term-cursor-pos [row col]
  (->>
    (sh/sh "tput" "cup" (str (int row)) (str (int col)))
    :out
    (print)))

(def state
  (atom
    {
     :k1    30
     :rot-x 0
     :rot-z 0
     }))

(defn state-update [dt]
  ; (swap! state update :k1 + (* dt 2))
  (swap! state update :rot-x + (* dt (/ Math/PI 10)))
  (swap! state update :rot-z + (* dt (/ Math/PI 12)))
  ; (swap! state update :dcol (partial + (* 10 dt)))
  ; (swap! state update :drow (partial + (* 10 dt)))
  )

(defn x-proj [x ooz]
  (Math/round
    (+ (/ (:cols conf) 2)
      (* (:k1 @state) x ooz))))

(defn y-proj [y ooz]
  (Math/round
    (+ (/ (:rows conf) 2)
      (* (:k1 @state) y ooz))))

(defn draw []
  (term-reset)

  ; (term-cursor-pos
  ;   (Math/floor (mod (+ (:row @state) (:drow @state)) (:rows conf)))
  ;   (Math/floor (mod (+ (:col @state) (:dcol @state)) (:cols conf))))
  ; (print "WWW")

  (let [cos-rx (Math/cos (:rot-x @state))
        cos-rz (Math/cos (:rot-z @state))
        sin-rx (Math/sin (:rot-x @state))
        sin-rz (Math/sin (:rot-z @state))

        zbuf   (atom {})
        output (atom {})
        ]
    (doseq [theta (range 0 (* 2 Math/PI) (:theta-spacing conf))
            :let [cos-theta (Math/cos theta)
                  sin-theta (Math/sin theta)
                  rcos0 (* (:r1 conf) cos-theta)
                  rcos (+ (:r2 conf) rcos0)
                  rsin (* (:r1 conf) sin-theta) ]]
      (doseq [phi   (range 0 (* 2 Math/PI) (:phi-spacing conf))
              :let [cos-phi (Math/cos phi)
                    sin-phi (Math/sin phi)

                    z (+ (:k2 conf) (- (* cos-rx sin-phi rcos) (* rsin sin-rx)))

                    ooz (/ 1 z)

                    x (+ (* rsin cos-rx sin-rz)
                         (* rcos (+ (* sin-rx sin-phi sin-rz) (* cos-phi cos-rz))))

                    y (+ (* rsin cos-rx cos-rz)
                         (* rcos (- (* sin-rx sin-phi cos-rz) (* cos-phi sin-rz))))

                    ; norm (max
                    ;        (Math/abs (- (* rsin sin-rx) (* rcos0 cos-rx sin-phi)))
                    ;        (Math/abs (+ (* rsin cos-rx cos-rz) (* rcos0 (- (* sin-rx sin-phi cos-rz) (* cos-phi sin-rz)))))
                    ;        (Math/abs (+ (* rsin cos-rx sin-rz) (* rcos0 (+ (* sin-rx sin-phi sin-rz) (* cos-phi cos-rz))))))

                    ; L (- (/ (+ (* rsin cos-rx cos-rz)
                    ;            (* rcos0 (- (* sin-rx sin-phi cos-rz) (* cos-phi sin-rz))))
                    ;         norm)
                    ;      (/ (- (* rcos0 cos-rx sin-phi) (* rsin sin-rx))
                    ;         norm))
                    L (- (+ (* rsin cos-rx cos-rz)
                            (* rcos0 (- (* sin-rx sin-phi cos-rz) (* cos-phi sin-rz))))
                         (- (* rcos0 cos-rx sin-phi) (* rsin sin-rx))
                         )

                    xp (x-proj x ooz)
                    yp (y-proj y ooz)
                    ]]

        (when (and (< 0 L)
                   (> ooz (or (get-in @zbuf [xp yp]) 0))
                   (<= 0 xp (:cols conf))
                   (<= 0 yp (:rows conf)))
          ; (println L)
          (swap! zbuf assoc-in [xp yp] ooz)
          (swap! output assoc-in [xp yp] L))
        ))


    (doseq [[x ys] @output
            [y L] ys]
      (let [L (int (* 8 L))]
        (when (<= 0 L 11)
          (term-cursor-pos y x)
          (print (nth (:luminance-chars conf) L))))
      )
    )


  (term-cursor-pos 0 0)
  )

(defn clock []
  (/ (System/currentTimeMillis) 1000.0))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [tstate    (atom {:t1 (clock)})
        framerate (double (/ 1 20))]
    (while true
      (swap! tstate assoc :t2 (clock))
      (let [dt (- (:t2 @tstate) (:t1 @tstate))]
        (state-update dt)
        (draw)
        (flush)
        (swap! tstate assoc :t1 (:t2 @tstate))
        ; (Thread/sleep 100)
        ))))

(comment
  (:out (sh/sh "tput" "clear"))
  (clojure.pprint/pprint (:out (sh/sh "tput" "clear")))
  (term-cursor-pos 1 1)
  )
