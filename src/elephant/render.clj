(ns elephant.render
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]
            [elephant.state :as st])
  (:import [com.googlecode.lanterna TextColor$ANSI SGR]))

(def ^:private colors
  {:lightred TextColor$ANSI/RED_BRIGHT
   :lightblue TextColor$ANSI/BLUE_BRIGHT})

(def ^:private modifiers
  {:bold SGR/BOLD})

(defn ^:private unescape [text]
  (s/replace
    text
    #"&([^;]*);"
    (fn [[_ entity]]
      (if (.startsWith entity "#x")
        (str (char (Integer/parseInt (subs entity 2) 16)))
        (str \& entity \;)))))

(defn ^:private parse [text]
  (map unescape (s/split text #"<p>")))

(defn ^:private reflow [max-width text]
  (if (<= (count text) max-width)
    (list text)
    (let [i (or (s/last-index-of text \space max-width) max-width)
          space? (= \space (.charAt text i))
          p (if space? (inc i) i)]
      (cons
        (subs text 0 i)
        (lazy-seq (reflow max-width (subs text p)))))))

(defn ^:private debug! [text-graphics state]
  (doseq [[line i] (map vector (s/split-lines (with-out-str (pp/pprint state))) (range))]
    (.putString text-graphics 0 i line)))

(defn ^:private put-str! [text-graphics column row & elements]
  (loop [elems elements
         pos column]
    (if-let [element (first elems)]
      (if (vector? element)
        (let [old-color (.getForegroundColor text-graphics)
              old-mods (.getActiveModifiers text-graphics)
              cmd (first element)]
          (when (modifiers cmd)
            (.enableModifiers text-graphics (into-array SGR [(modifiers cmd)])))
          (when (colors cmd)
            (.setForegroundColor text-graphics (colors cmd)))
          (let [p (apply put-str! text-graphics pos row (rest element))]
            (.setForegroundColor text-graphics old-color)
            (.setModifiers text-graphics old-mods)
            (recur (rest elems) p)))
        (do
          (.putString text-graphics pos row element)
          (recur (rest elems) (+ pos (count element)))))
      pos)))

(defn render! [screen state]
  (.doResizeIfNecessary screen)
  (let [text-graphics (.newTextGraphics screen)
        size (.getTerminalSize screen)
        width (.getColumns size)
        height (.getRows size)]
    (.clear screen)
    (if (:debug? state)
      (debug! text-graphics state)
      (do
        (.putString text-graphics 0 0 "Elephant 0.0.1")
        (when-let [story-item ((:items state) (:current-story-id state))]
          (.putString text-graphics 1 2 (:title story-item))
          (.putString text-graphics 1 3 (str (:url story-item) "  o:open-link  p:print-link"))
          (.putString text-graphics 1 4 (format "%d points by %s on %d | %d comments"
                                                (:score story-item) (:by story-item)
                                                (:time story-item) (:descendants story-item))))
        (when-let [item (st/current-item state)]
          (.putString text-graphics 1 6 (format "%s on %d | comment /%s | %d children"
                                                (:by item) (:time item) (s/join \/ (:path state))
                                                (count (:kids item))))
          (.putString text-graphics 1 7 "H:root  h:parent  j:next-sibling  k:prev-sibling  l:first-child")
          (doseq [[line i] (map vector
                                (mapcat
                                  (partial reflow (- (:width state) 2))
                                  (interpose "" (parse (:text item))))
                                (drop 9 (range)))]
            (when (< i (- height 2))
              (.putString text-graphics 1 i line))))
        (.putString text-graphics 0 (dec height) "q:quit"))))
  (.refresh screen))


(comment
  (reflow 5 "hello")
  (reflow 5 "hello a")
  (reflow 5 "helloa"))
