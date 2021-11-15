(ns elephant.render
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]
            [elephant.state :as st])
  (:import [com.googlecode.lanterna TextColor$ANSI SGR]
           [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]
           [java.net URL]
           [java.util Random]))

(def ^:private colors
  {:lightred TextColor$ANSI/RED_BRIGHT
   :white TextColor$ANSI/WHITE
   :lightblack TextColor$ANSI/BLACK_BRIGHT
   :lightwhite TextColor$ANSI/WHITE_BRIGHT
   :lightblue TextColor$ANSI/BLUE_BRIGHT})

(def ^:private modifiers
  {:bold SGR/BOLD})

(defn ^:private unescape [text]
  (s/replace
    text
    #"&([^;]*);"
    (fn [[_ entity]]
      (cond
        (.startsWith entity "#x") (str (char (Integer/parseInt (subs entity 2) 16)))
        (= entity "quot") "\""
        (= entity "gt") ">"
        (= entity "lt") "<"
        (= entity "amp") "&"
        :else (str \& entity \;)))))

(defn ^:private strip-tags [text]
  (s/replace text #"<[^>]*>" ""))

(defn ^:private parse [text]
  (map (comp unescape strip-tags) (s/split text #"<p>")))

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
  (doseq [[line i] (map vector (s/split-lines (with-out-str (pp/pprint state)))
                        (range 1 (dec (:height state))))]
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
          (.putString text-graphics pos row (str element))
          (recur (rest elems) (+ pos (count (str element))))))
      pos)))

(defn ^:private key-hint [key-seq hint]
  [:nop [:bold [:lightblue key-seq]] ":" [:white hint]])


(defn ^:private shorten-url [url]
  (as-> (URL. url) $
      (.getHost $)
      (s/split $ #"\.")
      (take-last 2 $)
      (s/join "." $)))

(defn ^:private format-time [epoch-sec]
  (.format
    (.atZone (Instant/ofEpochSecond epoch-sec)
             (ZoneId/systemDefault))
    DateTimeFormatter/RFC_1123_DATE_TIME))

(defn item! [text-graphics state]
  (when-let [story-item ((:items state) (:current-story-id state))]
    (put-str! text-graphics 1 2 [:bold (:title story-item)])
    (put-str! text-graphics 1 3
              [:lightblack (shorten-url (:url story-item))] "  "
              (key-hint "o" "open-link") "  "
              (key-hint "p" "print-link"))
    (.putString text-graphics 1 4 (format "%d points by %s on %s | %d comments"
                                          (:score story-item) (:by story-item)
                                          (format-time (:time story-item))
                                          (:descendants story-item))))
  (when-let [item (st/current-item state)]
    (.putString text-graphics 1 6 (format "%s on %s | comment /%s | %d children"
                                          (:by item) (format-time (:time item))
                                          (s/join \/ (:path state))
                                          (count (:kids item))))
    (put-str! text-graphics 1 7
              (key-hint "H" "root") "  "
              (key-hint "h" "parent") "  "
              (key-hint "j" "next-sibling") "  "
              (key-hint "k" "prev-sibling") "  "
              (key-hint "l" "first-child"))
    (doseq [[line i] (map vector
                          (mapcat
                            (partial reflow (- (:width state) 2))
                            (interpose "" (parse (:text item))))
                          (drop 9 (range)))]
      (when (< i (- (:height state) 2))
        (.putString text-graphics 1 i line)))))

(defn phrase [random]
  (s/join \space (repeatedly (+ 5 (.nextInt random 7))
                             #(s/join (repeat (+ 2 (.nextInt random 6)) \#)))))

(defn index! [text-graphics ids state]
  (doseq [i (range (min 10 (bit-shift-right (- (:height state) 4) 1)))]
    (let [random (Random. i)
          item ((:items state) (get (state ids) i))
          link-keyseq (st/index-keyseq (* 2 i))
          comment-keyseq (st/index-keyseq (inc (* 2 i)))]
      (put-str! text-graphics 1 (+ 2 (* 2 i))
                (format "%2d. " (inc i))
                (if item
                  [:nop [:bold (:title item)] \space [:bold [:lightblue link-keyseq]]]
                  [:lightblack (phrase random)]))
      (put-str! text-graphics 7 (+ 3 (* 2 i))
                (if item
                  [:nop (:descendants item) " comments " [:bold [:lightblue comment-keyseq]]
                   " | " (:score item) " points by " (:by item) " on "
                   (format-time (:time item))
                   ]
                  [:lightblack (phrase random)])))))

(defn render! [screen state]
  (when (and (:width state) (:height state))
    (.doResizeIfNecessary screen)
    (.clear screen)
    (let [text-graphics (.newTextGraphics screen)]
      (.putString text-graphics 0 0 "Elephant 0.0.3")
      (case (if (:debug? state) :debug (:view state))
        :debug (debug! text-graphics state)
        :item (item! text-graphics state)
        (index! text-graphics :best-ids state))
      (put-str! text-graphics 0 (dec (:height state)) (key-hint "q" "quit"))
      (put-str! text-graphics (- (:width state) 2) (dec (:height state)) (:input-prefix state)))
    (.refresh screen)))

(comment
  (reflow 5 "hello")
  (reflow 5 "hello a")
  (reflow 5 "helloa"))
