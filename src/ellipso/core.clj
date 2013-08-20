(ns ellipso.core)

(require '[clojure.core.async :as async])
(require '[serial-port :as comms])
(require '[ellipso.utils :as utils])

(def ^{:private true} seqid
  (let [ch (async/chan)]
    (async/go
      (loop [s 1]
             (async/>! ch s)
             (recur (mod (inc s) 256))))
    (fn [] (async/<!! ch))))

(defn- checksum [payload]
  (bit-xor (mod (reduce + payload) 256) 0xFF))

(defn- read-buffer
  "Handles reading the appropriate number of byte from the given InputStream"
  [stream length]
  (let [buffer (byte-array length)]
    (loop [offset 0 remain length]
      (when-not (zero? remain)
        (let [consumed (.read stream buffer offset remain)]
          (when-not (= consumed -1)
            (recur (+ offset consumed) (- remain consumed))))))
    buffer))

(defn send-to [sphero [device command & data] handler]
  (let [id      (seqid)
        dlen    (inc (count data))
        payload (concat [device command id dlen] data)
        xsum    (checksum payload)]
    ((:register-dispatch sphero) id (partial handler id))
    (async/>!! (:computer-sphero sphero) (concat [0xFF] payload [xsum]))))

(defmulti read-packet
  "Reads data from the given InputStream and builds a Sphero response packet."
  (fn [stream] (utils/signed->unsigned (.read stream))))

(defmethod read-packet 0xFF [stream]
  (let [mrsp     (.read stream)
        seqid    (.read stream)
        dlen     (dec (utils/signed->unsigned (.read stream)))
        data     (read-buffer stream dlen)
        checksum (.read stream)]
    (concat [0xFF mrsp seqid (inc dlen)] data)))

(defmethod read-packet 0xFE [stream]
  (let [idcode   (.read stream)
        dlen-msb (utils/signed->unsigned (.read stream))
        dlen-lsb (utils/signed->unsigned (.read stream))
        dlen     (dec (bit-or (bit-shift-left dlen-msb 8) dlen-lsb))
        data     (read-buffer stream dlen)
        checksum (.read stream)]
    (concat [0xFE idcode (inc dlen)] data [checksum])))

(def message-response-codes '(
              (0x00 OK           "Command succeeded")
              (0x01 EGEN         "General error")
              (0x02 ECHKSUM      "Received checksum failure")
              (0x03 EFRAG        "Received ommand fragment")
              (0x04 EBAD_CMD     "Unknown command ID")
              (0x05 EUNSUPP      "Command currently unsupported")
              (0x06 EBAD_MSG     "Bad message format")
              (0x07 EPARAM       "Parameter value(s) invalid")
              (0x08 EEXEC        "Failed to execute command")
              (0x09 EBAD_DID     "Unknown device ID")
              (0x31 POWER_NOGOOD "Voltage too low for reflash operation")
              (0x32 PAGE_ILLEGAL "Illegal page number provided")
              (0x33 FLASH FAIL   "Page did not reprogram correctly")
              (0x34 MA_CORRUPT   "Main application corrupt")
              (0x35 MSG_TIMEOUT  "Msg state machine timed out")))
(defn message_for [mrsp] (some #(when (= mrsp (first %)) %) message-response-codes))

(defn ensure-simple-response [id response]
  (let [payload  (concat [0x00 id 0x01])
        xsum     (checksum payload)
        expected (concat [0xFF] payload)]
    (when-not (= expected response)
      (println "Mismatch:" expected " vs " response)
      (println "MRSP:" (message_for (second response))))))

(defn- computer->sphero
  "Returns a core.async channel that represents the computer to Sphero communications path,
  along which Sphero command packets are sent."
  [port]
  (let [ch (async/chan)]
    (async/go
      (while true
        (comms/write port (byte-array (map utils/unsigned->signed (cons 0xFF (async/<! ch)))))))
    ch))

(defn- computer<-sphero
  "Returns a core.async channel that represents the Sphero to computer communications path,
  along which Sphero response packets are received."
  [port]
  (let [ch (async/chan)]
    (comms/listen port (fn [stream]
                         (.read stream) ; SOP1, always 0xFF
                         (async/>!! ch (map utils/signed->unsigned (read-packet stream)))))
    ch))

(defn- response-dispatcher
  "Configures dispatching of Sphero response packets received via the core.async channel
  specified.  Dispatching is handled via the sequence ID contained within the packet, or
  to the general handler specified."
  [ch async-lookup missing-handler]
  (let [seqid->handler (atom {})
        add-handler    (fn [seqid handler] (swap! seqid->handler #(assoc % seqid handler)))
        remove-handler (fn [seqid] (let [h (@seqid->handler seqid)] (swap! seqid->handler #(dissoc % seqid)) h))
        handler-for    (fn [packet] (or (and (= (first packet) 0xFE) (async-lookup packet)) (remove-handler (nth packet 2)) missing-handler))]
    (async/go
      (loop []
        (let [packet (async/<! ch)]
          (when-not (nil? packet)
            ((handler-for packet) packet)
            (recur)))))
    (fn [seqid handler]
      (swap! seqid->handler #(assoc % seqid handler)))
    ))

(defn- async-handler
  "Returns a pair of functions that can be used to register an asynchronous handler
  for a given response ID, and then subsequently look one up by response ID"
  [missing-handler]
  (let [id->handler (atom {})
        attach-handler (fn [id handler] (swap! id->handler #(assoc % id handler)))
        handler-for    (fn [packet] (or (@id->handler (second packet)) missing-handler))]
    [attach-handler handler-for]))

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

(defrecord Sphero [port computer-sphero sphero-computer register-dispatch register-async-dispatch])

(defn connect
  "Connects to the Sphero, returning a representation of the device through which all
  communication should pass."
  ([path]
   (let [sphero-port                              (comms/open path)
         computer-sphero                          (computer->sphero sphero-port)
         sphero-computer                          (computer<-sphero sphero-port)
         [attach-async-handler async-handler-for] (async-handler #(println "Missing async:" %))
         register-dispatch                        (response-dispatcher sphero-computer async-handler-for #(println "Missing:" %))]
     (Sphero. sphero-port computer-sphero sphero-computer register-dispatch attach-async-handler)))
  )

(defn disconnect
  "Disconnects the given Sphero."
  [sphero]
  (map #(async/close! (% sphero)) [:computer-sphero :sphero-computer])
  (comms/close (:port sphero)))
