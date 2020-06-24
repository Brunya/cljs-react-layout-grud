(ns app.views
  (:require [reagent.core :as reagent :refer [atom]]
            ["react-grid-layout" :as GridLayout]))

(defn app []
  [:> GridLayout {:cols 5 :rowHeight 30 :width 1200}
   ^{:key "a"} [:div.a (:data-grid {:x 0 :y 0 :w 1 :h 2 :static true}) "a"]
   ^{:key "b"} [:div.b (:data-grid {:x 1 :y 0 :w 3 :h 2 :minW 2 :maxW 4}) "b"]
   ^{:key "c"} [:div.c (:data-grid {:x 4 :y 0 :w 1 :h 2}) "c"]])
