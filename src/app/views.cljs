(ns app.views
  (:require [reagent.core :as reagent :refer [atom]]
            ["react-grid-layout" :as GridLayout]
            ["chart.js" :as Chart]))

(defn app []
  [:> GridLayout {:cols 5 :rowHeight 200 :width (-> js/screen .-availWidth)}

   ^{:key "a"} [:div.kartya {:data-grid {:x 0 :y 0 :w 1 :h 2}}
                [:div.pageName "Page Name"][:div.bigNumber "999"][:div.active "Active"][:div.activeCount "54"]]

   ^{:key "b"} [:div.kartya {:data-grid {:x 1 :y 0 :w 1 :h 2}}
                [:div.pageName "Page Name"][:div.bigNumber "69"][:div.active "Active"][:div.activeCount "54"]]

   ^{:key "c"} [:div.kartya {:data-grid {:x 2 :y 0 :w 1 :h 2}}
                [:div.pageName "Page Name"][:div.bigNumber "69"][:div.active "Active"][:div.activeCount "54"]]

   ^{:key "d"} [:div.kartya2 {:data-grid {:x 3 :y 0 :w 1 :h 1}}[:div.newOld [:div.allSites "New User"] [:div.bigNumber2 "69"] [:div.allViews "In the last 24 hour"]]
                                                              [:div.newOld [:div.allSites "Old User"] [:div.bigNumber2 "69"] [:div.allViews "In the last 24 hour"]]]


   ^{:key "e"} [:div.kartya2 {:data-grid {:x 0 :y 2 :w 3 :h 2}}[:div.allCounter
                                                                [:div.allSites "All Sites"][:div.bigNumber2 "69"][:div.allViews "All Views"]]
                                                              [:div.moreElements [:div.allDetails
                                                                                  [:div.daily [:div.allSites "All Sites"][:div.bigNumber2 "69"][:div.allViews "Daily"]]
                                                                                  [:div.weekly [:div.allSites "All Sites"][:div.bigNumber2 "69"][:div.allViews "Weekly"]]
                                                                                  [:div.monthly [:div.allSites "All Sites"][:div.bigNumber2 "69"][:div.allViews "Monthly"]]][:div.allGraph]]]

    ^{:key "f"} [:div.kartya3 {:data-grid {:x 0 :y 4 :w 2.5 :h 2}}[:div.cryptoDetails [:div.cryptoName "BTC"][:div.cryptoData [:div.cryptoPrice [:div.cryptoNumber "2924000,00"][:div.cryptoVault "USD"]][:div.cryptoChange [:div.cryptoIncdec "3002,25"][:div.cryptoVault "USD"]]]][:div.cryptoGraph]]])
