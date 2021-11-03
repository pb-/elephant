(ns elephant.commands
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [clj-http.client :as client]))

(defmulti execute-command!
  "Execute command and return a collection of emitted events."
  :type)

(defmethod execute-command! :default
  [])

(defmethod execute-command! :exit [command]
  (System/exit 0))

(defmethod execute-command! :http-get [command]
  [{:type :responded
    :context (:context command)
    :data (json/read-str (:body (client/get (:url command)))
                         :key-fn keyword)}])

(defmethod execute-command! :dump-state [command]
  (spit "/tmp/elephant-state-dump.edn"
        (with-out-str (pp/pprint (:state command))))
  [])
