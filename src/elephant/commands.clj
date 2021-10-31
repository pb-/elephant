(ns elephant.commands)

(defmulti execute-command!
  "Execute command and return a collection of emitted events."
  :type)

(defmethod execute-command! :default
  [])

(defmethod execute-command! :exit [command]
  (System/exit 0))
