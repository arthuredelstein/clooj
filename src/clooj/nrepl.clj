(ns clooj.nrepl
  (:import (java.io BufferedReader InputStreamReader))
  (:require [clojure.tools.nrepl :as nrepl]
            [clojure.java.io :as io]))

;https://github.com/clojure/tools.nrepl

;; Repl protocol

(defprotocol Repl
  (evaluate [this code channel] "Evaluate code in a named channel.")
  (close [this] "Stop the repl instance."))

;; nrepl handling

(defn connect-nrepl
  "Connect to an nrepl port. Return a user session and
   a control session."
  [port out-writer]
  (let [conn (nrepl/connect :port port)
        client (nrepl/client conn 1000)]
    {:port port
     :connection conn
     :client client
     :user (nrepl/new-session client)
     :control (nrepl/new-session client)
     :out out-writer}))

(defn disconnect-nrepl
  "Disconnects from an nrepl port."
  [{:keys [connection]}]
  (.close connection))

(defn nrepl-eval
  "Evaluate nrepl code, where session-type is either :user or :control."
  [nrepl-connection code session-type]
  (let [results (nrepl/message
                  (:client nrepl-connection)
                  {:op :eval :code (str "(do " code ")")
                   :session (session-type nrepl-connection)})
        promised-value (promise)]
    (println results)
    (future (doseq [result results]
              (when-let [out (:out result)]
                (binding [*out* (:out nrepl-connection)]
                  (print out)))
              (when-let [value (:value result)]
                (deliver promised-value (read-string value)))))
    @promised-value))

(defn nrepl
  "Connects to an nrepl, returning a Repl instance."
  [port out-writer]
  (let [nrepl (connect-nrepl port out-writer)]
    (reify Repl
      (evaluate [_ code channel]
        (nrepl-eval nrepl code channel))
      (close [_]
        (disconnect-nrepl nrepl)))))

;; lein repl

(defn lein-repl-process
  "Start an external lein repl process."
  [project-path]
  (let [command ["lein" "repl"]]
    (->
      (doto (ProcessBuilder. command)
        (.redirectErrorStream true)
        (.directory (io/file (or project-path "."))))
      .start)))

(defn process-reader
  "Create a buffered reader from the output of a process."
  [process]
  (-> process
      .getInputStream
      InputStreamReader.
      BufferedReader.))

(defn lein-nrepl-port-number
  "Takes the first line printed to stdout from a lein repl process
   and returns the nrepl port number."
  [out-line]
  (when-let [port-str (second (re-find #"port\s(\d+)" out-line))]
    (Long/parseLong port-str)))

(defn lein-repl-start
  "Start an external lein repl process, and connect
   to it via nrepl."
  [project-path out-writer]
  (let [process (lein-repl-process project-path)
        lines (line-seq (process-reader process))
        port (lein-nrepl-port-number (first (drop-while nil? lines)))]
    {:nrepl (nrepl port out-writer)
     :process process}))
    
(defn lein-repl-stop
  "Disconnect from the nrepl connection and destroy the lein repl process."
  [{:keys [process nrepl]}]
  (.close nrepl)
  (.destroy process))

(defn lein-repl
  "Creates and connect to a lein repl,
   returning a Repl instance."
  [project-path out-writer]
  (let [repl (lein-repl-start project-path out-writer)]
    (reify Repl
      (evaluate [_ code channel]
        (.evaluate (:nrepl repl) code channel))
      (close [_]
        (lein-repl-stop repl)))))
