(ns ellipso.commands)

(require '[ellipso.utils :as utils])
(require '[ellipso.core :as core])
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

(defn- attach-asynchronous-handler
  "Returns a command that will attach the given handler for the specified asynchronous
  response ID."
  [id callback]
  (fn [sphero] ((:register-async-dispatch sphero) id callback) sphero))

(defn- detach-asynchronous-handler
  "Returns a command that will detach the handler currently registered for the asynchronous
  response ID specified."
  [id]
  (fn [sphero] ((:register-async-dispatch sphero) id nil) sphero))

; The on-board devices of the Sphero ball
(def ^{:private true} CORE 0x00)
(def ^{:private true} SPHERO 0x02)

; Asynchronous message IDs
(def ^{:private true} POWER-NOTIFICATION 0x01)
(def ^{:private true} LEVEL-1-DIAGNOSTICS 0x02)
(def ^{:private true} SENSOR-DATA 0x03)
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

; ********************************************************************************************
; Commands that are more informational
; ********************************************************************************************
(defn ping
  "Returns a command that will send a ping command to the Sphero."
  []
  (fn [sphero] (core/send-to sphero [CORE 0x01] core/ensure-simple-response)))

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
  (fn [id response]
    (let [payload (drop 4 response)
          padded  (if (< (count payload) 0x0B) (concat payload [0x00 0x00]) payload)]
      (callback (apply ->Version padded)))))

(defn version
  "Returns a command that will retrieve the version information from the Sphero, handling
  a Version record to the callback function passed."
  [callback]
  (fn [sphero]
    (core/send-to sphero [CORE 0x02] (version-handler callback))))

(defn sleep
  "Returns a command that will put the Sphero to sleep, optionally waking in a specified
  number of seconds."
  ([] (sleep 0))
  ([wake-in]
   (let [payload (concat [CORE 0x22] (utils/int->bytes 2 wake-in) [0x00 0x00 0x00])]
     (fn [sphero] (core/send-to sphero payload core/ensure-simple-response)))))

(defn roll-timeout
  "Returns a command that will cause the Sphero motion to timeout after the specified number
  of milliseconds"
  [timeout]
  (let [payload (concat [SPHERO 0x34] (utils/int->bytes 2 timeout))]
    (fn [sphero] (core/send-to sphero payload core/ensure-simple-response))))

(defn stabilisation
  "Returns a command that will either turn on or off the automatic stabilisation of the
  Sphero."
  ([] (stabilisation true))
  ([stabilise]
   (fn [sphero]
     (core/send-to sphero [SPHERO 0x02 (if stabilise 0x01 0x00)] core/ensure-simple-response))))

(defn power-notifications
  "Returns a command that will enable power notifications from the Sphero and install the
  specified callback function as the handler for these.  If no callback is passed then the
  power notifications are disabled."
  ([]
   (let [detach (detach-asynchronous-handler POWER-NOTIFICATION)]
     (fn [sphero] (core/send-to (detach sphero) [CORE 0x21 DISABLE] core/ensure-simple-response))))
  ([callback]
   (let [attach (attach-asynchronous-handler POWER-NOTIFICATION callback)]
     (fn [sphero] (core/send-to (attach sphero) [CORE 0x21 ENABLE] core/ensure-simple-response)))))

(defn pre-sleep
  "Returns a command that will attach the given callback for the pre-sleep warning that
  the Sphero sends.  If the callback is unspecified then the currently registered callback
  is removed."
  ([]         (detach-asynchronous-handler PRE-SLEEP-WARNING))
  ([callback] (attach-asynchronous-handler PRE-SLEEP-WARNING callback)))

(defn perform-level-1-diagnostics
  "Returns a command that will cause the Sphero to generate level 1 diagnostic information,
  which will then be sent to the callback.  After the diagnostics have been received the
  callback will be removed from the device."
  [callback]
  (fn [sphero]
    (let [handler (fn [packet]
                    (let [no-header (drop 3 packet)
                          no-checksum (take (dec (count no-header)) no-header)]
                      (callback (apply str (map char no-checksum)))
                      ((detach-asynchronous-handler LEVEL-1-DIAGNOSTICS) sphero)))
          attach  (attach-asynchronous-handler LEVEL-1-DIAGNOSTICS handler)]
      (core/send-to (attach sphero) [CORE 0x40] core/ensure-simple-response))))

; ********************************************************************************************
; Commands that have a physical affect on the Sphero, like moving & flashing
; ********************************************************************************************
(defn back-led
  "Returns a command that will set the brightness of the back LED on the Sphero, turning it
  off if no value is specified."
  ([] (back-led 0x00))
  ([led] (fn [sphero] (core/send-to sphero [SPHERO 0x21 led] core/ensure-simple-response))))

(defn colour
  "Returns a command that will change the colour of the Sphero based on the value passed.  The
  value passed can be a 32-bit integer or 3 bytes representing the RGB values."
  ([value] (apply colour (utils/int->bytes 3 value)))
  ([red green blue]
   (fn [sphero] (core/send-to sphero [SPHERO 0x20 red green blue 0x00] core/ensure-simple-response))))

(defn heading
  "Returns a command that will reset the Sphero's zero heading based on the one specified."
  [degrees]
  (let [payload (concat [SPHERO 0x01] (utils/int->bytes 2 degrees))]
    (fn [sphero] (core/send-to sphero payload core/ensure-simple-response))))
(requires-valid-heading #'heading)

(defn roll
  "Returns a command that will cause the Sphero to roll at the given speed in the given heading.
  Heading can only be between 0 and 359."
  [speed heading]
  (let [payload (concat [SPHERO 0x30 speed] (utils/int->bytes 2 heading) [0x01])]
    (fn [sphero] (core/send-to sphero payload core/ensure-simple-response))))
(requires-valid-heading #'roll 1)

(defn stop
  "Returns a command that will cause the Sphero to come to a complete halt."
  []
  (fn [sphero] (core/send-to sphero [SPHERO 0x03 0x00 0x00 0x00 0x00] core/ensure-simple-response)))

(def OFF 0x00)
(def FORWARD 0x01)
(def REVERSE 0x02)
(def BRAKE 0x03)
(def IGNORE 0x04)

(defn raw-motor
  "Returns a command that will directly set the motors of the Sphero."
  [left right]
  (let [payload (concat [SPHERO 0x33] left right)]
    (fn [sphero] (core/send-to sphero payload core/ensure-simple-response))))

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
