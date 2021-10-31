(ns elephant.state)

(defn initial []
  {:ticks 1})

(defmulti update-state
  "Apply event to state and return a vector of new state and collection of commands."
  (fn [state event] (:type event)))

(defmethod update-state :resized [state event]
  [(assoc state
          :width (:width event)
          :height (:height event))
   []])

(defmethod update-state :default [state event]
  [state []])

(defmethod update-state :ticked [state event]
  [(update state :ticks inc) []])

(defmethod update-state :input-read [state event]
  (if (= (:character event) \q)
    [state [{:type :exit}]]
    [state []]))
