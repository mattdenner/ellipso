(ns ellipso.utils)

(defn int->bytes
  "Sometimes we need to take an integer and split it into a number of bytes."
  [number-of-bytes value]
  (map #(bit-and (bit-shift-right value (* 8 %)) 0xFF) (reverse (range 0 number-of-bytes))))

(def unsigned->signed
  (comp byte (fn [b] (if (bit-test b 7) (dec (- b 0xFF)) b))))

(def signed->unsigned
  (comp int (fn [b] (if (< b 0) (+ (inc b) 0xFF) b))))
