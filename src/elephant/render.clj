(ns elephant.render
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]
            [elephant.state :as st]))

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
    (cons text nil)  ;; is this the right way to create a one-element seq?!
    (let [i (or (s/last-index-of text \space max-width) max-width)
          space? (= \space (.charAt text i))
          p (if space? (inc i) i)]
      (cons
        (subs text 0 i)
        (lazy-seq (reflow max-width (subs text p)))))))

(defn debug [text-graphics state]
  (doseq [[line i] (map vector (s/split-lines (with-out-str (pp/pprint state))) (range))]
    (.putString text-graphics 0 i line)))

(defn render! [screen state]
  (.doResizeIfNecessary screen)
  (let [text-graphics (.newTextGraphics screen)]
    (.clear screen)
    (if (:debug? state)
      (debug text-graphics state)
      (do
        (.putString text-graphics 1 1 (str (:ticks state)))
        (.putString text-graphics 1 2 (str (:width state)))
        (.putString text-graphics 1 3 (str (:height state)))
        (.putString text-graphics 1 4 (str (count (:items state))))
        (when-let [item (st/current-item state)]
          (doseq [[line i] (map vector
                                (mapcat
                                  (partial reflow (dec (:width state)))
                                  (interpose "" (parse (:text item))))
                                (drop 4 (range)))]
            (when (< i (dec (:height state)))
              (.putString text-graphics 1 i line))))))
    (.refresh screen)))

(comment
  (reflow 5 "hello")
  (reflow 5 "hello a")
  (reflow 5 "helloa"))
