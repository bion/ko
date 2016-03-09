(ns ko.nrt
  (:require [clojure.java.io :refer [file output-stream]]
            [overtone.osc.peer :as ot-peer]
            [ko.scheduling :refer [quant-to-timestamp schedule-measure calc-beat-dur-ms]]
            [overtone.core :as ot]
            [byte-streams]
            [overtone.sc.defaults :refer [foundation-groups*]]
            [overtone.libs.deps :refer [satisfy-deps]]
            [overtone.sc.machinery.server.connection :as ot-connection]
            [overtone.sc.machinery.server.comms :as ot-comms]))

(defn setup-groups []
  (let [overtone-group (ot/group "Overtone" :head 0)
        timing-group (ot/group "Overtone Timing" :head overtone-group)
        input-group (ot/group "Overtone Inputs" :after timing-group)
        root-group (ot/group "Overtone Root" :after input-group)
        user-group (ot/group "Overtone User" :head root-group)
        safe-pre-default-group (ot/group "Overtone Safe Pre Default" :head user-group)
        default-group (ot/group "Overtone Default" :after safe-pre-default-group)
        safe-post-default-group (ot/group "Overtone Safe Post Default" :after default-group)
        output-group (ot/group "Overtone Output" :after root-group)
        monitor-group (ot/group "Overtone Monitor" :after output-group)]
    (swap! foundation-groups* assoc
           :overtone-group          overtone-group
           :timing-group            timing-group
           :input-group             input-group
           :root-group              root-group
           :user-group              user-group
           :safe-pre-default-group  safe-pre-default-group
           :default-group           default-group
           :safe-post-default-group safe-post-default-group
           :output-group            output-group
           :monitor-group           monitor-group)))

(defn- record-message [peer-obj buf]
  (let [message (java.nio.ByteBuffer/allocate (.limit buf))]
    (.put message buf 0 (.limit buf))
    (.flip message)
    (println "one")
    (println (.limit message))
    (println (byte-streams/convert message String))
    (println (.limit message))
    (println "two")
    (swap! (:sent-messages peer-obj) #(conj % message))))

(defn- pacify-group-create [msg]
  (ot/event "/n_go" msg))

(defn- pacify-server-sync [msg]
  (ot/event "/synced"))

(defn get-buf-osc-msg [buf]
  (let [msg (overtone.osc.decode/osc-decode-packet buf)]
    (.flip buf)
    msg))

(defn send-fn [peer-obj buf]
  (let [msg (get-buf-osc-msg buf)]
    (println msg)
    (cond (= "/g_new" (:path msg))
          (pacify-group-create msg)

          (= "/sync" (:path msg))
          (pacify-server-sync msg))

    (record-message peer-obj buf)))

(defn nrt-peer []
  (with-meta
    (assoc (ot-peer/peer)
           :sent-messages (atom [])
           :send-fn send-fn)
    {:type ::client}))

(defn init-nrt
  "Trick overtone into thinking a server is listening,
  actually just captures all generated OSC messages."
  []
  (let [peer (nrt-peer)]
    (dosync
     (ref-set ot-comms/server-osc-peer* peer)
     (ref-set ot-connection/connection-status* :connected))
    (setup-groups)))

(defn teardown-nrt []
  (dosync
   (ref-set ot-comms/server-osc-peer* nil)
   (ref-set ot-connection/connection-status* :disconnected)))

(defn nrt-play-score [input-score]
  (loop [score input-score
         current-time 0.0]
    (let [measure (first score)
          {:keys [beat-dur beats-per-bar]} (meta measure)
          beat-dur-ms (calc-beat-dur-ms beat-dur)
          next-score (rest score)
          next-bar-timestamp (+ current-time beat-dur-ms)
          next-cycle-timestamp (+ current-time (* beat-dur-ms beats-per-bar))]

      (schedule-measure measure next-bar-timestamp beat-dur)

      (if-not (empty? next-score)
        (recur (rest score) next-cycle-timestamp)))))

;; TODO add empty osc message to end of file
(defn write [peer-obj filename]
  (with-open [out (output-stream (file filename))]
    (doseq [message (deref (:sent-messages peer-obj))
            raw-msg (.array message)] ;; get byte[]
      (.write out raw-msg))))

(defn nrt-write-score-to-osc [score filename]
  (nrt-play-score score)
  (write @ot-comms/server-osc-peer* filename))
