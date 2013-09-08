(ns ellipso.commands)

(require '[ellipso.utils :as utils] :reload)
(require '[ellipso.core :as core] :reload)
(require '[dire.core :as dire])

; ********************************************************************************************
; Utility and support definitions
; ********************************************************************************************
(defn- requires-valid-heading
  "Ensures that the given argument is a valid heading"
  ([f] (requires-valid-heading f 0))
  ([f heading-arg]
   (dire/with-precondition! f
     :invalid-degrees
     (fn [& args]
       (let [degrees (nth args heading-arg nil)]
         (or (nil? degrees)
             (and (>= degrees 0) (< degrees 360))))))))

; Asynchronous message IDs
(def ^{:private true} POWER-NOTIFICATION 0x01)
(def ^{:private true} LEVEL-1-DIAGNOSTICS 0x02)
(def ^{:private true} CONFIG-BLOCK 0x04)
(def ^{:private true} PRE-SLEEP-WARNING 0x05)
(def ^{:private true} MACRO-MARKERS 0x06)
(def ^{:private true} COLLISION-DETECTED 0x07)
(def ^{:private true} ORBBASIC-PRINT 0x08)
(def ^{:private true} ORBBASIC-ERROR-ASCII 0x09)
(def ^{:private true} ORBBASIC-ERROR-BINARY 0x0A)
(def ^{:private true} SELF-LEVEL 0x0B)
(def ^{:private true} GYRO-AXIS-LIMIT-EXCEEDED 0x0C)

; Booleans for the packets
(def ^{:private true} DISABLE 0x00)
(def ^{:private true} ENABLE 0x01)

(defn- boolean->byte [b] (if b ENABLE DISABLE))

; ********************************************************************************************
; Commands that are more informational
; ********************************************************************************************
(defn ping
  "Returns a command that will send a ping command to the Sphero."
  []
  (fn [sphero] (core/send-to sphero [core/CORE 0x01] core/ensure-simple-response)))

(defrecord Version [record-version
                    model-number
                    hardware-version
                    main-sphero-version-major
                    main-sphero-version-minor
                    boot-loader-version-nibble
                    orbbasic-version-nibble
                    macro-version-nibble
                    api-version-major
                    api-version-minor])
(defn- version-handler
  "Returns a function that handles the version response packet from the Sphero.  Prior to
  version 1.20 the api-version-major and api-version-minor values were unavailable, so we
  need to handle that by padding those responses with zeroes."
  [callback]
  (fn [id packet]
    (let [payload (:data packet)
          padded  (if (< (count payload) 0x0B) (concat payload [0x00 0x00]) payload)]
      (callback (apply ->Version padded)))))

(defn version
  "Returns a command that will retrieve the version information from the Sphero, handling
  a Version record to the callback function passed."
  [callback]
  (fn [sphero]
    (core/send-to sphero [core/CORE 0x02] (version-handler callback))))

(defn sleep
  "Returns a command that will put the Sphero to sleep, optionally waking in a specified
  number of seconds."
  ([] (sleep 0))
  ([wake-in]
   (let [payload (concat [core/CORE 0x22] (utils/short->bytes wake-in) [0x00 0x00 0x00])]
     (fn [sphero] (core/send-to sphero payload core/ensure-simple-response)))))

(defn roll-timeout
  "Returns a command that will cause the Sphero motion to timeout after the specified number
  of milliseconds"
  [timeout]
  (let [payload (concat [core/SPHERO 0x34] (utils/short->bytes timeout))]
    (fn [sphero] (core/send-to sphero payload core/ensure-simple-response))))

(defn stabilisation
  "Returns a command that will either turn on or off the automatic stabilisation of the
  Sphero."
  ([] (stabilisation true))
  ([stabilise]
   (fn [sphero]
     (core/send-to sphero [core/SPHERO 0x02 (if stabilise 0x01 0x00)] core/ensure-simple-response))))

(def ^{:doc "States that come back from the power-notifications"}
  power-states [:charging :ok :low :critical])

(defn power-notifications
  "Returns a command that will enable power notifications from the Sphero and install the
  specified callback function as the handler for these.  If no callback is passed then the
  power notifications are disabled."
  ([]
   (let [detach (core/detach-asynchronous-handler POWER-NOTIFICATION)]
     (fn [sphero] (core/send-to (detach sphero) [core/CORE 0x21 DISABLE] core/ensure-simple-response))))
  ([callback]
   (let [power-info (fn [packet] (nth power-states (first (:data packet)) :unknown))
         attach     (core/attach-asynchronous-handler POWER-NOTIFICATION (comp callback power-info))]
     (fn [sphero] (core/send-to (attach sphero) [core/CORE 0x21 ENABLE] core/ensure-simple-response)))))

(defn pre-sleep
  "Returns a command that will attach the given callback for the pre-sleep warning that
  the Sphero sends.  If the callback is unspecified then the currently registered callback
  is removed."
  ([]         (core/detach-asynchronous-handler PRE-SLEEP-WARNING))
  ([callback] (core/attach-asynchronous-handler PRE-SLEEP-WARNING callback)))

(defn perform-level-1-diagnostics
  "Returns a command that will cause the Sphero to generate level 1 diagnostic information,
  which will then be sent to the callback.  After the diagnostics have been received the
  callback will be removed from the device."
  [callback]
  (fn [sphero]
    (let [handler (fn [{data :data}]
                    (callback (apply str (map char data)))
                    ((core/detach-asynchronous-handler LEVEL-1-DIAGNOSTICS) sphero))
          attach  (core/attach-asynchronous-handler LEVEL-1-DIAGNOSTICS handler)]
      (core/send-to (attach sphero) [core/CORE 0x40] core/ensure-simple-response))))

(defrecord PermanentOptions [value
                             keep-awake-on-charger
                             vector-drive
                             self-level-on-charger
                             back-led-on
                             motion-timeout
                             demo-mode])

(defn permanent-options
  "Returns a command that will call the callback function with the permanent options from
  the Sphero."
  [callback]
  (fn [sphero]
    (core/send-to sphero [core/SPHERO 0x36]
                  (fn [_ {data :data}]
                    (let [value    (utils/int<-bytes data)
                          settings (map #(bit-test (bit-shift-right value %) 0) (range 0 6))]
                      (callback (apply ->PermanentOptions (cons value settings))))))))

(defn- permanent-option [bit default]
  (let [value  (bit-set 0x00 bit)
        on     #(bit-or value %)
        off    #(bit-and (bit-not value) % 0x00FFFF)]
    (fn option-helper
      ([] (option-helper default))
      ([enable]
       (let [f (if enable on off)]
         (fn [sphero]
           (core/send-to sphero [core/SPHERO 0x36]
                    (fn [_ {data :data}]
                      (let [value   (utils/int<-bytes data)
                            payload (concat [core/SPHERO 0x35] (utils/int->bytes (f value)))]
                        (core/send-to sphero payload core/ensure-simple-response)))))
         )))))

(def keep-awake-on-charger (permanent-option 0 false)) ; 0x01
(def vector-drive          (permanent-option 1 false)) ; 0x02
(def self-level-on-charger (permanent-option 2 false)) ; 0x04
(def back-led-on           (permanent-option 3 true))  ; 0x08
(def motion-timeouts       (permanent-option 4 true))  ; 0x10
(def demo-mode             (permanent-option 5 false)) ; 0x20

(defn stabilization
  "Returns a command that will enable or disable the stabilisation of the Sphero."
  ([] (stabilization true))
  ([state]
   (fn [sphero]
     (core/send-to sphero [core/SPHERO 0x02 (boolean->byte state)] core/ensure-simple-response))))

; ********************************************************************************************
; Commands that have a physical affect on the Sphero, like moving & flashing
; ********************************************************************************************
(defn back-led
  "Returns a command that will set the brightness of the back LED on the Sphero, turning it
  off if no value is specified."
  ([] (back-led 0x00))
  ([led] (fn [sphero] (core/send-to sphero [core/SPHERO 0x21 led] core/ensure-simple-response))))

(defn colour
  "Returns a command that will change the colour of the Sphero based on the value passed.  The
  value passed can be a 32-bit integer or 3 bytes representing the RGB values."
  ([value] (apply colour (utils/triple-octets->bytes value)))
  ([red green blue]
   (fn [sphero] (core/send-to sphero [core/SPHERO 0x20 red green blue 0x00] core/ensure-simple-response))))

(defn heading
  "Returns a command that will reset the Sphero's zero heading based on the one specified."
  [degrees]
  (let [payload (concat [core/SPHERO 0x01] (utils/short->bytes degrees))]
    (fn [sphero] (core/send-to sphero payload core/ensure-simple-response))))
(requires-valid-heading #'heading)

(defn roll
  "Returns a command that will cause the Sphero to roll at the given speed in the given heading.
  Heading can only be between 0 and 359."
  [speed heading]
  (let [payload (concat [core/SPHERO 0x30 speed] (utils/short->bytes heading) [0x01])]
    (fn [sphero] (core/send-to sphero payload core/ensure-simple-response))))
(requires-valid-heading #'roll 1)

(def OFF     0x00)
(def FORWARD 0x01)
(def REVERSE 0x02)
(def BRAKE   0x03)
(def IGNORE  0x04)

(defn raw-motor
  "Returns a command that will directly set the motors of the Sphero."
  [left right]
  (let [payload (concat [core/SPHERO 0x33] left right)]
    (fn [sphero] (core/send-to sphero payload core/ensure-simple-response))))

(defn stop
  "Returns a command that will cause the Sphero to come to a complete halt."
  []
  (fn [sphero] (core/send-to sphero [core/SPHERO 0x03 0x00 0x00 0x00 0x00] core/ensure-simple-response)))
;  (raw-motor [FORWARD 0x00] [FORWARD 0x00]))

(defn left-motor
  "Returns a command that will directly set the left motor of the Sphero"
  [mode power]
  (raw-motor [mode power] [IGNORE 0x00]))

(defn right-motor
  "Returns a command that will directly set the right motor of the Sphero."
  [mode power]
  (raw-motor [IGNORE 0x00] [mode power]))

(def CLOCKWISE identity)
(def ANTI-CLOCKWISE reverse)

(defn spin
  "Returns a command that will spin the Sphero in the given direction, at the given rate."
  [direction rate]
  (apply raw-motor (direction [[FORWARD rate] [REVERSE rate]])))

; ********************************************************************************************
; Utility functions
; ********************************************************************************************
(defn pause
  "Returns a command that will pause the process for a while.  This is primarily used so that
  other commands can have a chance to play out, for example when flashing the Sphero colours."
  [timeout]
  (fn [sphero] (Thread/sleep timeout)))

(defn execute
  "Sends the given command to the given Sphero, returning the resulting Sphero"
  [sphero command]
  (command sphero)
  sphero)
