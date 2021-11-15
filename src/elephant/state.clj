(ns elephant.state)

(defn initial []
  {:view :best
   :items {}
   :input-prefix ""})

(def index-keyseq
  ["a" "s" "d" "f" "g"
   "ha" "hs" "hd" "hf" "hg" "hh" "hj" "hk" "hl"
   "ja" "js" "jd" "jf" "jg" "jh" "jj" "jk" "jl"
   "ka" "ks" "kd" "kf" "kg" "kh" "kj" "kk" "kl"
   "la" "ls" "ld" "lf" "lg" "lh" "lj" "lk"])

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

(defmethod update-state :root-navigated [state event]
  [(assoc state :path [(first (:path state))]) []])

(defmethod update-state :parent-navigated [state event]
  (if (> (count (:path state)) 1)
    [(update state :path pop) []]
    [state []]))

(defmethod update-state :prev-sibling-navigated [state event]
  (if-let [id (prev-sibling-id state)]
    [(update-in state [:path (dec (count (:path state)))] dec)
     (get-unless-cached state id)]
    [state []]))

(defmethod update-state :next-sibling-navigated [state event]
  (if-let [id (next-sibling-id state)]
    [(update-in state [:path (dec (count (:path state)))] inc)
     (get-unless-cached state id)]
    [state []]))

(defmethod update-state :first-child-navigated [state event]
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

(defmethod update-state :exit-requested [state event]
  [state [{:type :exit}]])

(defmethod update-state :debug-toggled [state event]
  [(update state :debug? not) []])

(defmethod update-state :state-dumped [state event]
  [state [{:type :dump-state
           :state state}]])

(defmethod update-state :index-navigated [state event]
  [(assoc state :view :best) []])

(defmethod update-state :link-opened [state event]
  [state [{:type :open-link
           :url (:url ((:items state) (:current-story-id state)))}]])

(defmethod update-state :story-link-opened [state event]
  [state [{:type :open-link
           :url (:url ((:items state) (get (:best-ids state) (:index event))))}]])

(defmethod update-state :story-opened [state event]
  (update-state state {:type :switch-story
                       :story-id (get (:best-ids state) (:index event))}))

(def ^:private generic-keys
  {\q :exit-requested
   \e :debug-toggled
   \D :state-dumped})

(def ^:private story-keys
  {\H :root-navigated
   \h :parent-navigated
   \k :prev-sibling-navigated
   \j :next-sibling-navigated
   \l :first-child-navigated
   \b :index-navigated
   \o :link-opened})

(def ^:private index-keys
  (reduce
    (fn [m [i keyseq]]
      (assoc-in m (seq keyseq)
                {:type (if (even? i) :story-link-opened :story-opened)
                 :index (bit-shift-right i 1)}))
    {}
    (map-indexed vector index-keyseq)))

(defmethod update-state :input-read [state event]
  (let [current-keys (merge generic-keys (if (= (:view state) :best) index-keys story-keys))
        character (:character event)
        full-input (str (:input-prefix state) character)
        state-input-reset (assoc state :input-prefix "")]
    (loop [input full-input
           keymap current-keys]
      (if (empty? input)
        [(assoc state :input-prefix full-input)]
        (let [node (keymap (first input))]
          (cond
            (nil? node) [state-input-reset []]
            (keyword? node) (update-state state-input-reset {:type node})
            (char? (ffirst node)) (recur (rest input) node)
            :else (update-state state-input-reset node)))))))
