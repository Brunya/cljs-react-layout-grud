(ns app.views
  (:require [reagent.core :as reagent :refer [atom]]
            ["react-grid-layout" :as rgl :default GridLayout]
            [packery :as Packery]))

(defn app []
  (js/console.log GridLayout)
  [:> GridLayout {:cols 12 :rowHeight 30 :width 1200}
   [:div (:data-grid {:x 0 :y 0 :w 1 :h 2 :static true}) "a"]
   [:div (:data-grid {:x 1 :y 0 :w 3 :h 2 :minW 2 :maxW 4}) "b"]
   [:div (:data-grid {:x 4 :y 0 :w 1 :h 2}) "c"]])
