(ns app.views
  (:require [reagent.core :as reagent :refer [atom]]
            ["react-grid-layout" :as GridLayout]
            [clojure.string :refer [join]]
            ["chart.js" :as Chart]
            [clojure.string :as str]))

;BOGUS DATA--------------------------------------------------------------------------------------------

(def data (atom {:feri {:browserName "Firefox" :browserLang "Lovari" :device "Mobil" :os "Linux" :time 1593460888909 :cookie? "Yes" :siteLocation "zgen.hu"} :valaki1 {:browserName "Chromium" :time 1591459607082 :device "Tablet" :os "MacOS" :cookie? "No" :siteLocation "zgen.hu"} :bela {:browserName "Opera" :time 1593359607082 :browserLang "English" :device "PC" :os "Windows" :cookie? "Yes" :siteLocation "Zegen.com"}}))
;(def datasum (atom {:browserName (list "Chromium"  "Chrome" "Edge" "Explorer" "Explorer") :valami (list "asd" "aasd" "aaasd")}))
(def colorvector (atom ["#00ADB5" "#E8F8F5" "#7ee8ed" "#D1F2EB" "#76D7C4" "#48C9B0" "#1ABC9C" "#17A589" "#148F77" "#117864" "#0E6251" "#117864"]))
;:siteLocation '() :osName '() :cpuCores '() :browserHeight '() :browserWidth '() :deviceManufacturer '() :screenHeight '() :screenWidth '() :cookies? '() :cookies '() :colorDepth '() :pixelDepth '() :pathName '() :clientTime '() :referrer '() :prevSites '() :protocol '() :browserLang '()

;------------------------------------------------------------------------------------------------------
;---------------------------------------------FUNCTIONS------------------------------------------------
;------------------------------------------------------------------------------------------------------

(defn labelvector [key]
  (let [list (atom ())]
    (doseq [i (keys @data)]
      (reset! list (conj @list (get-in @data [i (keyword key)]))))
    (let [val (atom {})]
      (doseq [i (range (count @list))]
        (when (<= 0 ((keyword (nth @list i)) @val)) (swap! val update (keyword (nth @list i)) inc)))
      (into [] (keys @val)))))

(defn datavector [key]
  (let [list (atom ())]
    (doseq [i (keys @data)]
      (reset! list (conj @list (get-in @data [i (keyword key)]))))
    (let [val (atom {})]
      (doseq [i (range (count @list))]
        (when (<= 0 ((keyword (nth @list i)) @val)) (swap! val update (keyword (nth @list i)) inc)))
      (into [] (vals @val)))))

(defn timecounter [mode]
  (let [list (atom ())]
    (doseq [i (keys @data)]
      (reset! list (conj @list (get-in @data [i :time]))))
    (let [val (atom ())]
      (doseq [i (range (count @list))]
        (when (>= (cond (= mode "day") 86400000
                        (= mode "week") 604800000
                        (= mode "active") 600000
                        (= mode "month") 2629746000) (- (.getTime (js/Date.)) (nth @list i))) (reset! val (conj @val (nth @list i)))))
      (str (count @val)))))

(defn time-to []
  (let [list (atom ())]
    (doseq [i (keys @data)]
      (reset! list (conj @list (get-in @data [i :time]))))
    (let [val (atom {:1 0 :2 0 :3 0})]
      (doseq [i @list]
        (when (> 1 (/ (- (.getTime (js/Date.)) i) 86400000)) (swap! val update-in [:1] inc))
        (when (> 2 (/ (- (.getTime (js/Date.)) i) 86400000)) (swap! val update-in [:2] inc))
        (when (> 3 (/ (- (.getTime (js/Date.)) i) 86400000)) (swap! val update-in [:3] inc))
        (when (> 4 (/ (- (.getTime (js/Date.)) i) 86400000)) (swap! val update-in [:4] inc))
        (when (> 5 (/ (- (.getTime (js/Date.)) i) 86400000)) (swap! val update-in [:5] inc)))
      (into [] (vals @val)))))

(defn days []
  (let [val (atom ())]
    (into [] (join (list (.getMonth (js/Date.)) (.getDate (js/Date.)))))))

(defonce timer (atom (js/Date.)))

(defonce time-updater (js/setInterval
                       #(reset! timer (js/Date.)) 1000))

(defn clock []
  (let [time-str (-> @timer .toTimeString (str/split " ") first)]
    [:div.example-clock
     {:style {:color "#00ADB5"}}
     time-str]))


;------------------------------------------------------------------------------------------------------
;---------------------------------------------CHARTS---------------------------------------------------
;------------------------------------------------------------------------------------------------------

;DEVICES CHART-----------------------------------------------------------------------------------------

(defn show-revenue-chart-devices
  []
  (let [context (.getContext (.getElementById js/document "rev-chartjs-devices") "2d")
        chart-data {:type "pie"
                    :data {
                           :labels (labelvector "device")
                           :datasets [{:data (datavector "device")
                                       :backgroundColor @colorvector}]}}]
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
                           :labels (labelvector "browserName")
                           :datasets [{:data (datavector "browserName")
                                       :backgroundColor @colorvector}]}}]
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
                           :labels (labelvector "os")
                           :datasets [{:data (datavector "os")
                                       :backgroundColor @colorvector}]}}]
      (js/Chart. context (clj->js chart-data))))


(defn rev-chartjs-component-os
  []
  (reagent/create-class
    {:component-did-mount #(show-revenue-chart-os)
     :display-name        "chartjs-component-os"
     :reagent-render      (fn []
                            [:canvas {:id "rev-chartjs-os" :width "100%" :height "100%"}])}))

;ALL CHART -----------------------------------------------------------------

(defn show-revenue-chart-line
  []
  (let [context (.getContext (.getElementById js/document "rev-chartjs-line") "2d")
        chart-data {:type "line"
                    :data {
                           :labels [(join (list (.getMonth (js/Date.)) "." (.getDate (js/Date.))))]
                          ;
                           :datasets [{:data (time-to)}]}

                    :options {
                              :scales {
                                       :xAxes {
                                               :display false}}}}]

      (js/Chart. context (clj->js chart-data))))


(defn rev-chartjs-component-line
  []
  (reagent/create-class
    {:component-did-mount #(show-revenue-chart-line)
     :display-name        "chartjs-component-line"
     :reagent-render      (fn []
                            [:canvas {:id "rev-chartjs-line" :width "auto" :height "100%"}])}))


;COOKIE CHART-----------------------------------------------------------------------------------------

(defn show-revenue-chart-cookie
  []
  (let [context (.getContext (.getElementById js/document "rev-chartjs-cookie") "2d")
        chart-data {:type "bar"
                    :data {
                           :labels (labelvector "cookie?")
                           :datasets [{
                                       :data (datavector "cookie?")
                                       :backgroundColor @colorvector}]}}]
      (js/Chart. context (clj->js chart-data))))


(defn rev-chartjs-component-cookie
  []
  (reagent/create-class
    {:component-did-mount #(show-revenue-chart-cookie)
     :display-name        "chartjs-component-cookie"
     :reagent-render      (fn []
                            [:canvas {:id "rev-chartjs-cookie" :width "100%" :height "100%"}])}))

;CRYPTO CHART-----------------------------------------------------------------------------------------

(defn show-revenue-chart-crypto
  []
  (let [context (.getContext (.getElementById js/document "rev-chartjs-crypto") "2d")
        chart-data {:type "line"
                    :data {}}]






      (js/Chart. context (clj->js chart-data))))


(defn rev-chartjs-component-crypto
  []
  (reagent/create-class
    {:component-did-mount #(show-revenue-chart-crypto)
     :display-name        "chartjs-component-crypto"
     :reagent-render      (fn []
                            [:canvas {:id "rev-chartjs-crypto" :width "auto" :height "90%"}])}))

;--------------------------------------------------------------------------------------------------

(defn app []

  [:> GridLayout {:cols 5 :rowHeight 210 :width (-> js/screen .-availWidth)}

;One Page Card
   ^{:key "a"} [:div.kartya {:data-grid {:x 0 :y 0 :w 1 :h 2}}
                [:div.pageName "All views"][:div.bigNumber {:class [(when (< 2 (count (str (count @data)))) "longnumber")]} (count @data)][:div.active "Active"][:div.activeCount (timecounter "active")]]

;New/Old Users Card
   ^{:key "b"} [:div.kartya2 {:data-grid {:x 3 :y 0 :w 1 :h 1}}[:div.newOld [:div.allSites "New User"] [:div.bigNumber2 {:class [(when (< 2 (count (timecounter "day"))) "longnumber")]} (timecounter "day")] [:div.allViews "In the last 24 hour"]]
                                                              [:div.newOld [:div.allSites "Old User"] [:div.bigNumber2 "69"] [:div.allViews "In the last 24 hour"]]]
;Main Static Card
   ^{:key "i"} [:div.kartya4 {:data-grid {:x 1 :y 0 :w 1.5 :h 1}}[:div.static [:div.staticHeader [:div.staticName [:div.staticName2 "ZGEN"][:div.staticName3 "analytics"]][:div.staticHeaderButtons [:label.switch [:intput {:type "checkbox"}][:span.slider.round]]]][:div.staticTime [#(clock)]]]]

;Broesers Card
   ^{:key "c"} [:div.kartya {:data-grid {:x 1 :y 0 :w 1 :h 2}}[:div.browser [:div.browserName "Browsers"][:div.browserGraph [(rev-chartjs-component-browser)]]]]

;Devices Card
   ^{:key "d"} [:div.kartya {:data-grid {:x 2 :y 0 :w 1 :h 2}}[:div.devices [:div.devicesName "Devices"][:div.devicesGraph [#(rev-chartjs-component-devices)]]]]

;OS Card
   ^{:key "e"} [:div.kartya {:data-grid {:x 3 :y 0 :w 1 :h 2}}[:div.os [:div.osName "Operating System"][:div.osGraph [#(rev-chartjs-component-os)]]]]

;Cookie Card
   ^{:key "f"} [:div.kartya {:data-grid {:x 4 :y 0 :w 1 :h 2}}[:div.cookie [:div.cookieName "Cookie Usage"][:div.cookieGraph [#(rev-chartjs-component-cookie)]]]]

;All Sites Card
   ^{:key "g"} [:div.kartya2 {:data-grid {:x 0 :y 2 :w 3 :h 2}}[:div.allCounter
                                                                [:div.allSites "All Sites"][:div.bigNumber2 (count @data)][:div.allViews "All Views"]]
                                                              [:div.moreElements [:div.allDetails
                                                                                  [:div.daily [:div.allSites "All Sites"][:div.bigNumber2 (timecounter "day")][:div.allViews "Daily"]]
                                                                                  [:div.weekly [:div.allSites "All Sites"][:div.bigNumber2 (timecounter "week")][:div.allViews "Weekly"]]
                                                                                  [:div.monthly [:div.allSites "All Sites"][:div.bigNumber2 (timecounter "month")][:div.allViews "Monthly"]]][:div.allGraph [#(rev-chartjs-component-line)]]]]

;Crypto Card
    ^{:key "h"} [:div.kartya3 {:data-grid {:x 0 :y 4 :w 3 :h 2}}[:div.crypto [:div.cryptoDetails [:div.cryptoName "BTC"][:div.cryptoData [:div.cryptoPrice [:div.cryptoNumber "2924000,00"][:div.cryptoVault "USD"]][:div.cryptoChange [:div.cryptoIncdec "3002,25"][:div.cryptoVault "USD"]]]][:div.cryptoGraph [:div.cryptoGraph2 [#(rev-chartjs-component-crypto)]]]]]])


(defn app1 []
  (-> js/screen .-height))




;       (for [i (range 1)])]])
;       ^{:key i} [:div.kartya {:class [(join (list "c" i))] :data-grid {:x (rand-int 4) :y (rand-int 4) :w 1 :h (if (< i 3) 2 3)}} [:div.devices {:class [(join (list "h" i))]} "Szoveg"] [:div.devicesGraph "Megszoveg" [#()]]]
;        ^{:key i} [:div.kartya {:data-grid {:x (rand-int 4) :y (rand-int 4) :w 1 :h (if (< i 3) 2 3)}} [:div {:class [(join (list "t" i))]} [:div {:class [(join (list "h" i))]} (str (nth @hlist i))] [:div {:class [(join (list "g" i))]}[#(rev-chartjs-component-os)]]]])]])
