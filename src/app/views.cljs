(ns app.views
  (:require [reagent.core :as reagent :refer [atom]]
            ["react-grid-layout" :as GridLayout]
            ["chart.js" :as Chart]))

;DEVICES CHART-----------------------------------------------------------------------------------------

(defn show-revenue-chart-devices
  []
  (let [context (.getContext (.getElementById js/document "rev-chartjs-devices") "2d")
        chart-data {:type "pie"
                    :data {
                           :datasets [{:data [100 300 400]
                                       :backgroundColor "#00ADB5"}]}}]
      (js/Chart. context (clj->js chart-data))))


(defn rev-chartjs-component-devices
  []
  (reagent/create-class
    {:component-did-mount #(show-revenue-chart-devices)
     :display-name        "chartjs-component-devices"
     :reagent-render      (fn []
                            [:canvas {:id "rev-chartjs-devices" :width "100%" :height "100%"}])}))

;BROWSER CHART-----------------------------------------------------------------------------------------

(defn show-revenue-chart-browser
  []
  (let [context (.getContext (.getElementById js/document "rev-chartjs-browser") "2d")
        chart-data {:type "doughnut"
                    :data {
                           :datasets [{:data [100 200 300 400]
                                       :backgroundColor "#00ADB5"}]}}]
      (js/Chart. context (clj->js chart-data))))


(defn rev-chartjs-component-browser
  []
  (reagent/create-class
    {:component-did-mount #(show-revenue-chart-browser)
     :display-name        "chartjs-component-browser"
     :reagent-render      (fn []
                            [:canvas {:id "rev-chartjs-browser" :width "100%" :height "100%"}])}))

;OS CHART-----------------------------------------------------------------------------------------

(defn show-revenue-chart-os
  []
  (let [context (.getContext (.getElementById js/document "rev-chartjs-os") "2d")
        chart-data {:type "doughnut"
                    :data {
                           :datasets [{:data [200 300 400 500]
                                       :backgroundColor "#00ADB5"}]}}]
      (js/Chart. context (clj->js chart-data))))


(defn rev-chartjs-component-os
  []
  (reagent/create-class
    {:component-did-mount #(show-revenue-chart-os)
     :display-name        "chartjs-component-os"
     :reagent-render      (fn []
                            [:canvas {:id "rev-chartjs-os" :width "100%" :height "100%"}])}))

;ALL CHART-----------------------------------------------------------------------------------------

(defn show-revenue-chart-all
  []
  (let [context (.getContext (.getElementById js/document "rev-chartjs-all") "2d")
        chart-data {:type "line"
                    :data {
                           :datasets [{:data [2000 3000 4000 5000 6000]
                                       :backgroundColor "#00ADB5"}]}}]
      (js/Chart. context (clj->js chart-data))))


(defn rev-chartjs-component-all
  []
  (reagent/create-class
    {:component-did-mount #(show-revenue-chart-all)
     :display-name        "chartjs-component-all"
     :reagent-render      (fn []
                            [:canvas {:id "rev-chartjs-all" :width "100%" :height "100%"}])}))

;COOKIE CHART-----------------------------------------------------------------------------------------

(defn show-revenue-chart-cookie
  []
  (let [context (.getContext (.getElementById js/document "rev-chartjs-cookie") "2d")
        chart-data {:type "bar"
                    :data {
                           :datasets [{:data [100 200 300 400 500]
                                       :label "Allowed"
                                       :backgroundColor "#02d9e3"}
                                      {:data [100 200 300 400 500]
                                       :label "Not Allowed"
                                       :backgroundColor "#00ADB5"}]}}]
      (js/Chart. context (clj->js chart-data))))


(defn rev-chartjs-component-cookie
  []
  (reagent/create-class
    {:component-did-mount #(show-revenue-chart-cookie)
     :display-name        "chartjs-component-cookie"
     :reagent-render      (fn []
                            [:canvas {:id "rev-chartjs-cookie" :width "100%" :height "100%"}])}))

;--------------------------------------------------------------------------------------------------

(defn app []
  [:> GridLayout {:cols 5 :rowHeight 200 :width (-> js/screen .-availWidth)}

   ^{:key "a"} [:div.kartya {:data-grid {:x 0 :y 0 :w 1 :h 2}}
                [:div.pageName "Page Name"][:div.bigNumber "999"][:div.active "Active"][:div.activeCount "54"]]

   ^{:key "d"} [:div.kartya2 {:data-grid {:x 3 :y 0 :w 1 :h 1}}[:div.newOld [:div.allSites "New User"] [:div.bigNumber2 "69"] [:div.allViews "In the last 24 hour"]]
                                                              [:div.newOld [:div.allSites "Old User"] [:div.bigNumber2 "69"] [:div.allViews "In the last 24 hour"]]]

   ^{:key "g"} [:div.kartya2 {:data-grid {:x 2 :y 0 :w 1.5 :h 1.5}}[:div.browserDetails [:div.browserCard "Browsers"][:div.browser1 "Google Chrome"][:div.browser2 "Mozzilla Firefox"][:div.browser3 "Opera GX"]][:div.barGraph [#(rev-chartjs-component-browser)]]]

   ^{:key "h"} [:div.kartya {:data-grid {:x 2 :y 0 :w 1 :h 2}}[:div.devices [:div.devicesName "Devices"][:div.devicesGraph [#(rev-chartjs-component-devices)]]]]

   ^{:key "i"} [:div.kartya {:data-grid {:x 2 :y 0 :w 1 :h 2}}[:div.os [:div.osName "Operating System"][:div.osGraph [#(rev-chartjs-component-os)]]]]

   ^{:key "j"} [:div.kartya {:data-grid {:x 2 :y 0 :w 1 :h 2}}[:div.cookie [:div.cookieName "Cookie Usage"][:div.cookieGraph [#(rev-chartjs-component-cookie)]]]]

   ^{:key "e"} [:div.kartya2 {:data-grid {:x 0 :y 2 :w 3 :h 2}}[:div.allCounter
                                                                [:div.allSites "All Sites"][:div.bigNumber2 "69"][:div.allViews "All Views"]]
                                                              [:div.moreElements [:div.allDetails
                                                                                  [:div.daily [:div.allSites "All Sites"][:div.bigNumber2 "69"][:div.allViews "Daily"]]
                                                                                  [:div.weekly [:div.allSites "All Sites"][:div.bigNumber2 "69"][:div.allViews "Weekly"]]
                                                                                  [:div.monthly [:div.allSites "All Sites"][:div.bigNumber2 "69"][:div.allViews "Monthly"]]][:div.allGraph]]]

    ^{:key "f"} [:div.kartya3 {:data-grid {:x 0 :y 4 :w 3 :h 2}}[:div.crypto [:div.cryptoDetails [:div.cryptoName "BTC"][:div.cryptoData [:div.cryptoPrice [:div.cryptoNumber "2924000,00"][:div.cryptoVault "USD"]][:div.cryptoChange [:div.cryptoIncdec "3002,25"][:div.cryptoVault "USD"]]]][:div.cryptoGraph [:div.cryptoGraph2]]]]])
