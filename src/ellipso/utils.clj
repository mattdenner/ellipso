(ns ellipso.utils)

(defn octets->bytes
  "Sometimes we need to take an integer and split it into a number of bytes."
  [number-of-bytes value]
  (map #(bit-and (bit-shift-right value (* 8 %)) 0xFF) (reverse (range 0 number-of-bytes))))

(defn octets<-bytes
  "Sometimes we need to convert a number of bytes into an int."
  [number-of-bytes values]
  (reduce #(bit-or (bit-shift-left %1 8) %2) 0x00 (take number-of-bytes values)))

(defn unsigned->signed [n]
  (let [sign-bit (dec (* n 8))
        zero     (octets<-bytes n (take n (repeat 0xFF)))]
    (fn [value]
      (if (bit-test value sign-bit) (dec (- value zero)) zero))))

(defn signed->unsigned [n]
  (let [sign-bit (dec (* n 8))
        zero     (octets<-bytes n (take n (repeat 0xFF)))]
    (fn [value]
      (if (bit-test value sign-bit) (+ (inc value) zero) zero))))

(defn byte->bytes [b] [(byte b)])
(defn byte<-bytes [[b & r]] (byte b))
(defn unsigned-byte->bytes [ub] [(byte (if (bit-test ub 7) (dec (- ub 0xFF)) ub))])
(defn unsigned-byte<-bytes [[b & r]] (if (bit-test b 7) (+ (inc (int b)) 0xFF) (int b)))

(def short->bytes         (partial octets->bytes 2))
(def short<-bytes         (partial octets<-bytes 2))
(def triple-octets->bytes (partial octets->bytes 3))
(def triple-octets<-bytes (partial octets<-bytes 3))
(def int->bytes           (partial octets->bytes 4))
(def int<-bytes           (partial octets<-bytes 4))

;(def short->bytes         unsigned-short->bytes)
;(def short<-bytes         (comp (unsigned->signed 2) unsigned-short<-bytes))
;(def triple-octets->bytes unsigned-triple-octets->bytes)
;(def triple-octets<-bytes (comp (unsigned->signed 3) unsigned-triple-octets<-bytes))
;(def int->bytes           unsigned-int->bytes)
;(def int<-bytes           (comp (unsigned->signed 4) unsigned-int<-bytes))
