(ns ellipso.utils)

(require '[dire.core :as dire])

(defn octets->bytes
  "Sometimes we need to take an integer and split it into a number of bytes."
  [number-of-bytes value]
  (map #(bit-and (bit-shift-right value (* 8 %)) 0xFF) (reverse (range 0 number-of-bytes))))

(defn octets<-bytes
  "Sometimes we need to convert a number of bytes into an int."
  [number-of-bytes values]
  (reduce #(bit-or (bit-shift-left %1 8) %2) 0x00 (take number-of-bytes values)))
(dire/with-precondition! #'octets<-bytes :invalid-byte-array
  (fn [number-of-bytes values] (= number-of-bytes (count values))))

(def short->bytes         (partial octets->bytes 2))
(def short<-bytes         (partial octets<-bytes 2))
(def triple-octets->bytes (partial octets->bytes 3))
(def triple-octets<-bytes (partial octets<-bytes 3))
(def int->bytes           (partial octets->bytes 4))
(def int<-bytes           (partial octets<-bytes 4))

(def unsigned->signed
  (comp byte (fn [b] (if (bit-test b 7) (dec (- b 0xFF)) b))))

(def signed->unsigned
  (comp int (fn [b] (if (< b 0) (+ (inc b) 0xFF) b))))
