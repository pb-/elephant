(ns elephant.state)

(defn initial []
  {:ticks 1
   :items {}})

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

(defn current-item-id [state]
  (loop [item ((:items state) (:current-story-id state))
         path (:path state)]
    (if-not (and item path)
      nil
      (let [item-id (nth (:kids item) (first path))
            rest-path (rest path)]
        (if (empty? rest-path)
          item-id
          (recur ((:items state) item-id) rest-path))))))

(defn current-item [state]
  ((:items state) (current-item-id state)))

(defmethod update-state :initialized [state event]
  [(assoc state
          :current-story-id 8863
          :path [0])
   [{:type :http-get
     :context :item
     :url "https://hacker-news.firebaseio.com/v0/item/8863.json"}]])

(defmethod update-state :responded [state event]
  (if (= :item (:context event))
    (let [item (:data event)
          s (assoc-in state [:items (:id item)] item)
          current-id (current-item-id s)
          commands (if-not ((:items s) current-id)
                     [{:type :http-get
                       :context :item
                       :url (format "https://hacker-news.firebaseio.com/v0/item/%d.json" current-id)}]
                     [])]
      [s commands])
    [state []]))

(defmethod update-state :ticked [state event]
  [(update state :ticks inc) []])

(defmethod update-state :input-read [state event]
  (case (:character event)
    \q [state [{:type :exit}]]
    \d [(update state :debug? not) []]
    \D [state [{:type :dump-state
                :state state}]]
    [state []]))
