(ns ellipso.sensor-data)

(require '[ellipso.core :as core])
(require '[ellipso.utils :as utils])

; ********************************************************************************************
; Utility and support definitions
; ********************************************************************************************
(def ^{:private true} SENSOR-DATA 0x03)

(defn- take-next-value-from-frame [name number-of-bytes]
  (fn [[frame sensor-data]]
    [(drop number-of-bytes frame)
     (assoc sensor-data name (utils/octets<-bytes number-of-bytes (take number-of-bytes frame)))]))

(defn- def-sensor-data
  [n & details]
  (let [mask    (reduce #(bit-set %1 %2) 0x00000000 (map first details))
        bit->fn (reduce (fn [h [b v s]]
                          (assoc h b
                                 (take-next-value-from-frame
                                  (keyword
                                   (str (name n) "-" (name v))) s))) {} details)]
    (fn
       ([]  mask)
       ([b] (get bit->fn b)))))

(defn combine-sensors
  "Combines the given sensor information so that it can be treated as a single entity."
  [& sensors]
  (fn
    ([]    (reduce #(bit-or %1 (%2)) 0x00000000 sensors))
    ([bit] (some #(% bit) sensors))))

(defn- handler-for
  "When dealing with the sensor data we need a function that will take a packet from the Sphero
  and process it so that we get a hash containing the requested sensor values."
  [sensors callback]
  (fn [{data :data}]
    (let [initial  [data {}]
          [_ data] (reduce (fn [memo bit-index] ((or (sensors bit-index) identity) memo))
                           initial
                           (reverse (range 0 32)))]
      (callback data))))

; ********************************************************************************************
; All of the sensor values appropriately grouped
; ********************************************************************************************
(def accelerometer-x (def-sensor-data :accelerometer-x [31 :raw 2] [15 :filtered 2]))
(def accelerometer-y (def-sensor-data :accelerometer-y [30 :raw 2] [14 :filtered 2]))
(def accelerometer-z (def-sensor-data :accelerometer-z [29 :raw 2] [13 :filtered 2]))
(def gyro-x          (def-sensor-data :gyro-x          [28 :raw 2] [12 :filtered 2]))
(def gyro-y          (def-sensor-data :gyro-y          [27 :raw 2] [11 :filtered 2]))
(def gyro-z          (def-sensor-data :gyro-z          [26 :raw 2] [10 :filtered 2]))
(def right-motor     (def-sensor-data :right-motor     [22 :back-emf-raw 2] [19 :pmw-raw 2] [06 :back-emf-filtered 2]))
(def left-motor      (def-sensor-data :left-motor      [21 :back-emf-raw 2] [20 :pmw-raw 2] [05 :back-emf-filtered 2]))
(def imu-pitch       (def-sensor-data :imu-pitch       [18 :filtered 2]))
(def imu-roll        (def-sensor-data :imu-roll        [17 :filtered 2]))
(def imu-yaw         (def-sensor-data :imu-yaw         [16 :filtered 2]))

; DO NOT USE FOR THE MOMENT!
;(def quaternion      0xF0000000)
;(def odometer        0x0C000000)
;(def accel-one       0x02000000)
;(def velocity        0x01800000)

; Useful groupings of sensor data
(def accelerometer-full (combine-sensors accelerometer-x accelerometer-y accelerometer-z))
(def gyro-full          (combine-sensors gyro-x gyro-y gyro-z))
(def motor-full         (combine-sensors left-motor right-motor))
(def imu-full           (combine-sensors imu-pitch imu-roll imu-yaw))
(def everything         (combine-sensors accelerometer-full gyro-full motor-full imu-full))
(def nothing            (fn ([] 0x00000000) ([bit] identity)))

; ********************************************************************************************
; Commands for dealing with sensor data
; ********************************************************************************************
; Reserved bits in the sensor data which we always mask out
(def ^{:private true} reserved-fields-mask  (bit-not 0x0380039F))
(def ^{:private true} reserved-fields-mask2 (bit-not 0xFF800000))

(defn data-streaming
  "Returns a command that enables data streaming from the Sphero for the given information.
  Note that only one data streaming method can be attached at any one time."
  ([sensors callback] (data-streaming sensors 0 callback))
  ([sensors packet-count callback]
   (let [masked-mask  (sensors)
         decoders     (remove nil? (map sensors (reverse (range 0 64))))
         handler      (handler-for sensors callback)

         masked-mask2 0x00000000
         payload      (concat [core/SPHERO 0x11 0x00 0x04 0x00 0x001]
                              (utils/int->bytes masked-mask)
                              [packet-count]
                              (utils/int->bytes masked-mask2))
         ]
     (fn [sphero]
       (let [attach (core/attach-asynchronous-handler SENSOR-DATA handler)]
         (core/send-to (attach sphero) payload core/ensure-simple-response))))))
