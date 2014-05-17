(ns ellipso.core)

(require '[clojure.core.async :as async])
(require '[serial-port :as comms])
(require '[ellipso.utils :as utils] :reload)
(require '[ellipso.statemachine :as statemachine] :reload)
(require '[clojure.stacktrace :as stacktrace])
(require '[clojure.string :as string])
(require '[clojure.java.shell :as shell])

(def ^{:private true} seqid
  (let [ch (async/chan)]
    (async/go
      (loop [s 1]
             (async/>! ch s)
             (recur (mod (inc s) 256))))
    (fn [] (async/<!! ch))))

(defn- checksum [payload]
  (bit-xor (mod (reduce + payload) 256) 0xFF))

(defn send-to [sphero [device command & data] handler]
  (let [id      (seqid)
        dlen    (inc (count data))
        payload (concat [device command id dlen] data)
        xsum    (checksum payload)]
    ((:register-dispatch sphero) id (partial handler id))
    (async/>!! (:computer-sphero sphero) (concat [0xFF] payload [xsum]))))

(defn ensure-simple-response [id received]
  (let [expected {:mrsp 0x00, :seqid id, :data []}]
    (when-not (= expected received)
      (println "Mismatch:" expected " vs " received))))

(defn- computer->sphero
  "Returns a core.async channel that represents the computer to Sphero communications path,
  along which Sphero command packets are sent."
  [port]
  (let [ch (async/chan)]
    (async/go
      (loop []
        (let [data (async/<! ch)]
          (comms/write port
                       (byte-array (map (fn [b]
                                          (if (bit-test b 7)
                                            (byte (dec (- b 0xFF)))
                                            (byte b)))
                                        (cons 0xFF data)))))
        (recur)))
    ch))

(defn packet-dispatcher [sphero-port packet-handler]
  (let [context (atom (statemachine/initial-context packet-handler))]
    (comms/on-byte
      sphero-port
      (fn [b]
        (try
          (swap! context #(statemachine/execute % b))
          (catch Exception e
            (println "Error" (.getName (.getClass e)) (.getMessage e))
            (stacktrace/print-stack-trace (stacktrace/root-cause e))))))))

(defn registry [frame->identifier single-call missing-handler]
  (let [identifier->handler (atom {})]
    (fn
      ([frame]
       (let [identifier (frame->identifier frame)
             h          (get @identifier->handler identifier missing-handler)]
         (when single-call (swap! identifier->handler #(dissoc % identifier)))
         h))

      ([identifier handler]
       (swap! identifier->handler #(assoc % identifier handler))))))

(defn type-registry [synchronous-registry asynchronous-registry missing-handler]
  (fn [type frame]
    (case type
      :synchronous  (synchronous-registry frame)
      :asynchronous (asynchronous-registry frame)
      missing-handler)))

(defn attach-asynchronous-handler
  "Returns a command that will attach the given handler for the specified asynchronous
  response ID."
  [id callback]
  (fn [sphero] ((:register-async-dispatch sphero) id callback) sphero))

(defn detach-asynchronous-handler
  "Returns a command that will detach the handler currently registered for the asynchronous
  response ID specified."
  [id]
  (fn [sphero] ((:register-async-dispatch sphero) id nil) sphero))

; The on-board devices of the Sphero ball
(def CORE 0x00)
(def SPHERO 0x02)

(defrecord Sphero [port computer-sphero register-dispatch register-async-dispatch])

(defn- standard-missing-handler
  "Missing packets are handled by a single function that either gets one packet, or an id
  and a packet.  This is the default behaviour, which simply dumps this error to stdout."
  ([packet]    (println "Unknown asynchronous packet:" packet) (flush))
  ([id packet] (println "Unknown synchronous packet:" packet)  (flush))
  )

(defn connect
  "Connects to the Sphero, returning a respresentation of the device through which all 
  communication should pass."
  [path]
  (let [sphero-port           (comms/open path)
        computer-sphero       (computer->sphero sphero-port)
        missing-handler       standard-missing-handler
        synchronous-registry  (registry (fn [frame] (:seqid frame)) true missing-handler)
        asynchronous-registry (registry (fn [frame] (:idcode frame)) false missing-handler)
        registry              (type-registry synchronous-registry asynchronous-registry missing-handler)]

    (packet-dispatcher
      sphero-port
      (fn [packet]
        (let [frame (:frame packet)
              handler (registry (:type packet) frame)]
          (println "handler" handler frame)
          (handler frame))))

    (Sphero. sphero-port computer-sphero synchronous-registry asynchronous-registry)))


(defn list-sphero-paths
  "Lists connected spheros"
  []
  (string/split (:out (shell/sh "bash" "-c" "ls /dev/tty.Sphero*")) #"\n"))

(defn connect-first
  "Connects to the first Sphero found. This is the easiest way
to get started, assuming you only have 1 spero connected at a time."
  []
  (connect (first (list-sphero-paths))))

(defn disconnect
  "Disconnects the given Sphero."
  [sphero]
  (map #(async/close! (% sphero)) [:computer-sphero :sphero-computer])
  (comms/close (:port sphero)))
