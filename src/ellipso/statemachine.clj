(ns ellipso.statemachine)

; ********************************************************************************************
; Context functions
; ********************************************************************************************
(declare initial-handler)

(defn- move-to
  ([context state]
   (move-to context (fn [_] true) state))

  ([context predicate state]
   (if (predicate context)
     (assoc context :handler state)
     context)))

(defn- finalise [context]
  (move-to (dissoc context :packet)
           initial-handler))

(defn- invalidate [context]
  (move-to context
           (fn [context b]
             (println "Invalid context" context) (flush)
             context)))

; ********************************************************************************************
; Packet (inside context) functions
; ********************************************************************************************
(defn- initialise-packet [context type]
  (assoc context
         :packet {:type type, :frame {:data []}}))

(defn- update-frame [context & args]
  (let [frame (:frame (:packet context))
        updated-frame (apply assoc frame args)]
    (assoc context
           :packet (assoc (:packet context)
                          :frame updated-frame))))

(defn- append-data [context b]
  (let [frame (:frame (:packet context))
        data  (concat (:data frame) [b])]
    (update-frame context :data data)))

(defn- frame-length [context]
  (count (:data (:frame (:packet context)))))

; ********************************************************************************************
; Support functions
; ********************************************************************************************
(defn- read-data-handler [dlen state-after-data]
  (if (zero? dlen)
    state-after-data
    (fn [context b]
      (move-to (append-data context b)
               (fn [c] (= dlen (frame-length c)))
               state-after-data))))

; ********************************************************************************************
; Statemachine functions
; ********************************************************************************************
; Pre-declare the state functions because it's easier to read in natural order!
(declare handle-sop2)
(declare handle-synchronous-mrsp)
(declare handle-synchronous-seqid)
(declare handle-synchronous-dlen)
(declare handle-asynchronous-idcode)
(declare handle-asynchronous-dlen)
(declare handle-checksum)

; Packet handling ...
(defn- handle-sop1 [context b]
  (println "handle-sop1" b)
  (case b
    0xFF (move-to context handle-sop2)
    (invalidate context)))

(defn- handle-sop2 [context b]
  (println "handle-sop2" b)
  (case b
    0xFF (move-to (initialise-packet context :synchronous)  handle-synchronous-mrsp)
    0xFE (move-to (initialise-packet context :asynchronous) handle-asynchronous-idcode)
    (invalidate context)))

(defn- handle-checksum [context b]
  (println "handle-checksum" b)
  ((:callback context) (:packet context))
  (finalise context))

; Synchronous message handling ...
(defn- handle-synchronous-mrsp [context b]
  (println "handle-synchronous-mrsp" b)
  (move-to (update-frame context :mrsp b) handle-synchronous-seqid))

(defn- handle-synchronous-seqid [context b]
  (println "handle-synchronous-seqid" b)
  (move-to (update-frame context :seqid b) handle-synchronous-dlen))

(defn- handle-synchronous-dlen [context dlen-plus-checksum]
  (println "handle-synchronous-dlen" dlen-plus-checksum)
  (let [dlen (dec dlen-plus-checksum)]
    (move-to context (read-data-handler dlen handle-checksum))))

; Asynchronous message handling ...
(defn- handle-asynchronous-idcode [context b]
  (println "handle-asynchronous-idcode" b)
  (move-to (update-frame context :idcode b) handle-asynchronous-dlen))

(defn- handle-asynchronous-dlen [context dlen-msb]
  (println "handle-asynchronous-dlen" dlen-msb)
  (move-to context
           (fn [context dlen-lsb]
             (println "handle-asynchronous-dlen" dlen-lsb)
             (let [dlen (dec (bit-or (bit-shift-left dlen-msb 8) dlen-lsb))]
               (move-to context (read-data-handler dlen handle-checksum))))))

; The initial state is always the initial state!
(def initial-handler handle-sop1)

; ********************************************************************************************
; Exposed statemachine interface
; ********************************************************************************************
(defn initial-context [callback]
  {:handler initial-handler, :callback callback})

(defn execute [context b]
  (let [h (:handler context)]
    (if (nil? h)
      (invalidate context)
      (h context b))))
