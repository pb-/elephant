(ns elephant.state)

(defn initial []
  {:view :best
   :items {}
   :input-prefix ""})

(def ^:private index-keymap
  {\a 0, \s 1, \d 2, \f 3, \g 4
   \h {\a 5, \s 6, \d 7, \f 8, \g 9, \h 10, \j 11, \k 12, \l 13}
   \j {\a 14, \s 15, \d 16, \f 17, \g 18, \h 19, \j 20, \k 21, \l 22}
   \k {\a 23, \s 24, \d 25, \f 26, \g 27, \h 28, \j 29, \k 30, \l 31}
   \l {\a 32, \s 33, \d 34, \f 35, \g 36, \h 37, \j 38, \k 39}})

(defn ^:private inverse-keymap
  ([keymap]
   (into {} (inverse-keymap keymap "")))
  ([keymap prefix]
   (mapcat
     (fn [[k v]]
       (let [key-seq (str prefix k)]
         (if (number? v)
           (list [v key-seq])
           (inverse-keymap v key-seq))))
     keymap)))

(def inverse-index-keymap (inverse-keymap index-keymap))

(defn lookup [keymap prefix]
  (let [v (keymap (first prefix))
        tail (rest prefix)]
    (when v
      (if (empty? tail)
        v
        (recur v tail)))))

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
  (first (:kids (current-item state))))

(defmethod update-state :initialized [state event]
  [state
   [{:type :http-get
     :context :best
     :url "https://hacker-news.firebaseio.com/v0/beststories.json"}]])

(defmethod update-state :responded [state event]
  (case (:context event)
    :item [(let [item (:data event)]
             (assoc-in state [:items (:id item)] item))
           []]
    :best (let [ids (into [] (take 10 (:data event)))]
            [(assoc state :best-ids ids)
             (for [id ids
                   :when (nil? ((:items state) id))]
               {:type :http-get
                :context :item
                :url (format "https://hacker-news.firebaseio.com/v0/item/%d.json" id)})])
    [state []]))

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

(defmethod update-state :switch-story [state event]
  (let [story-id (:story-id event)]
    [(assoc state
            :view :item
            :current-story-id story-id
            :path [0])
     (get-unless-cached
       state (first (:kids ((:items state) story-id))))]))

(defmethod update-state :input-read [state event]
  (let [c (:character event)]
    (case c
      \q [state [{:type :exit}]]
      \d [(update state :debug? not) []]
      \D [state [{:type :dump-state
                  :state state}]]
      (case (:view state)
        :item (case c
                \H (update-state state {:type :root})
                \h (update-state state {:type :parent})
                \k (update-state state {:type :prev-sibling})
                \j (update-state state {:type :next-sibling})
                \l (update-state state {:type :first-child})
                \b [(assoc state :view :best) []]
                [state []])
        :best (if-let [i (lookup index-keymap (str (:input-prefix state) c))]
                (if (number? i)
                  (let [story-id (get (:best-ids state) (bit-shift-right i 1))
                        s (assoc state :input-prefix "")]
                    (update-state s {:type :switch-story
                                     :story-id story-id}))
                  [(update state :input-prefix str c) []])
                [(assoc state :input-prefix "") []])
        [state []]))))
