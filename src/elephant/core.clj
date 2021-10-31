(ns elephant.core
  (:require [clojure.core.async :refer [go chan >! <!!]]
            [elephant.render :as r]
            [elephant.state :as s]
            [elephant.commands :as c])
  (:import [com.googlecode.lanterna.terminal DefaultTerminalFactory TerminalResizeListener]
           [com.googlecode.lanterna.screen TerminalScreen]))

(defn ^:private ticker [channel]
  (go (while true
        (Thread/sleep 1000)
        (>! channel {:type :ticked}))))

(defn ^:private input-reader [channel screen]
  (go (while true
        (>! channel {:type :input-read
                     :character (.getCharacter (.readInput screen))}))))

(defn ^:private make-resize-listener [channel]
  (reify TerminalResizeListener
    (onResized [_ _ new-size]
      (go (>! channel {:type :resized
                       :width (.getColumns new-size)
                       :height (.getRows new-size)})))))

(defn -main []
  (let [screen (TerminalScreen. (.createTerminal (DefaultTerminalFactory.)))]
    (try
      (.startScreen screen)
      (.setCursorPosition screen nil)
      (let [event-channel (chan)
            initial-state (s/initial)
            resize-listener (make-resize-listener event-channel)]
        (.addResizeListener (.getTerminal screen) resize-listener)
        (.onResized resize-listener nil (.getTerminalSize screen))
        (input-reader event-channel screen)
        (ticker event-channel)
        (r/render! screen initial-state)
        (go (>! event-channel {:type :initialized}))
        (loop [state initial-state]
          (let [event (<!! event-channel)
                [new-state commands] (s/update-state state event)]
            (r/render! screen new-state)
            (doseq [command commands]
              (go (doseq [event (c/execute-command! command)]
                    (>! event-channel event))))
            (recur new-state))))
      (finally
        (.close screen)))))
