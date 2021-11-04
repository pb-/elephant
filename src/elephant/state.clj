(ns elephant.state)

(defn initial []
  {:ticks 1
   :view :item
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

(defn lookup-item-id [state path]
  (loop [id (:current-story-id state)
         p path]
    (if (seq p)
      (recur (get (:kids ((:items state) id)) (first p)) (rest p))
      id)))

(defn lookup-item [state path]
  ((:items state) (lookup-item-id state path)))

(defn current-item-id [state]
  (lookup-item-id state (:path state)))

(defn current-item [state]
  (lookup-item state (:path state)))

(defn current-parent [state]
  (lookup-item state (pop (:path state))))

(defn prev-sibling-id [state]
  (let [parent (current-parent state)
        index (last (:path state))]
    (get (:kids parent) (dec index))))

(defn next-sibling-id [state]
  (let [parent (current-parent state)
        index (last (:path state))]
    (get (:kids parent) (inc index))))

(defn first-child-id [state]
  (get (:kids (current-item state)) 0))

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

(defn get-unless-cached [state id]
  (if ((:items state) id)
    []
    [{:type :http-get
       :context :item
       :url (format "https://hacker-news.firebaseio.com/v0/item/%d.json" id)}]))

(defmethod update-state :root [state event]
  [(assoc state :path [(first (:path state))]) []])

(defmethod update-state :parent [state event]
  (if (> (count (:path state)) 1)
    [(update state :path pop) []]
    [state []]))

(defmethod update-state :prev-sibling [state event]
  (if-let [id (prev-sibling-id state)]
    [(update-in state [:path (dec (count (:path state)))] dec)
     (get-unless-cached state id)]
    [state []]))

(defmethod update-state :next-sibling [state event]
  (if-let [id (next-sibling-id state)]
    [(update-in state [:path (dec (count (:path state)))] inc)
     (get-unless-cached state id)]
    [state []]))

(defmethod update-state :first-child [state event]
  (if-let [id (first-child-id state)]
    [(update state :path conj 0)
     (get-unless-cached state id)]
    [state []]))

(defmethod update-state :input-read [state event]
  (case (:character event)
    \q [state [{:type :exit}]]
    \d [(update state :debug? not) []]
    \D [state [{:type :dump-state
                :state state}]]
    \H (update-state state {:type :root})
    \h (update-state state {:type :parent})
    \k (update-state state {:type :prev-sibling})
    \j (update-state state {:type :next-sibling})
    \l (update-state state {:type :first-child})
    [state []]))
