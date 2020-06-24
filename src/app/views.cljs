(ns app.views
  (:require [reagent.core :as reagent :refer [atom]]
            ["react-grid-layout" :as GridLayout]))

(defn app []
  [:> GridLayout {:cols 5 :rowHeight 200 :width 1000}
   ^{:key "a"} [:div.kartya {:data-grid {:x 0 :y 0 :w 1 :h 2}}[:div.pageName "Page Name"][:div.bigNumber "69"][:div.active "Active"][:div.activeCount "54"]]
   ^{:key "x"} [:div.kartya {:data-grid {:x 0 :y 0 :w 3 :h 2}}]
   ^{:key "c"} [:div.kartya {:data-grid {:x 1 :y 0 :w 1 :h 2}}[:div.pageName "Page Name"][:div.bigNumber "69"][:div.active "Active"][:div.activeCount "54"]]
   ^{:key "d"} [:div.kartya {:data-grid {:x 1 :y 0 :w 1 :h 2}}[:div.pageName "Page Name"][:div.bigNumber "69"][:div.active "Active"][:div.activeCount "54"]]])
