(ns app.views
  (:require [reagent.core :as reagent :refer [atom]]
            ["react-grid-layout" :as GridLayout]
            [clojure.string :refer [join]]
            ["chart.js" :as Chart]
            [brave.cmc :as cmc]
            [clojure.string :as str]))

;BOGUS DATA--------------------------------------------------------------------------------------------

(def data (cmc/init {:apikey "testing-the-board" :host "cc.zgen.hu" :protocol :https :reagent? true}))
(def state (atom {:darkmode true}))
(def colorvector (atom ["#00ADB5" "#E8F8F5" "#7ee8ed" "#D1F2EB" "#76D7C4" "#48C9B0" "#1ABC9C" "#17A589" "#148F77" "#117864" "#0E6251" "#117864"]))
(def daylist (atom ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]))

;----------------------------------------------BOGUS DATA FUNCTIONS----------------------------------


(defn adatbazisadd []
  (let [newuser (atom {:key false})]
    (when (-> js/navigator .-cookieEnabled)
      (when (nil? (.getItem (.-localStorage js/window) "zgen")) ((reset! newuser {:key true})(.setItem (.-localStorage js/window) "zgen" (.getTime (js/Date.))))))
   (swap! data assoc (if (-> js/navigator .-cookieEnabled) (keyword (.getItem (.-localStorage js/window) "zgen")) (keyword (str (.getTime (js/Date.)))))
    {:newuser (:key @newuser)
     :browserName "SeaMonkey"
     :siteLocation (-> js/window .-location .-hostname)
     :osName (-> js/navigator .-platform)
     :cpuCores (-> js/navigator .-hardwareConcurrency)
     :deviceManufacturer (case (< (-> js/screen .-width) 768) "mobile" (= (-> js/screen .-width) 768) "tablet" (> (-> js/screen .-width) 768) "desktop")
     :screenHeight (-> js/screen .-height)
     :screenWidth (-> js/screen .-width)
     :cookies? (-> js/navigator .-cookieEnabled)
     :cookies (-> js/document .-cookie)
     :colorDepth (-> js/screen .-colorDepth)
     :pixelDepth (-> js/screen .-pixelDepth)
     :pathName (-> js/window .-location .-pathname)
     :clientTime (.Date js/window)
     :referrer (-> js/document .-referrer)
     :prevSites (-> js/history .-length)
     :protocol (-> js/window .-location .-protocol)
     :browserLang (-> js/navigator .-language)
     :time (.getTime (js/Date.))})))

(defn extraadd []
  (let [newuser (atom {:key true})]
    (swap! data assoc (keyword (str (.getTime (js/Date.)))) {:newuser (:key @newuser)
                                                             :browserName (nth (list "SeaMonkey" "Chrome" "Chromium" "Firefox" "Microsoft Edge" "Opera" "Safari" "Internet Explorer") (rand-int 8))
                                                             :siteLocation (-> js/window .-location .-hostname)
                                                             :osName (-> js/navigator .-platform)
                                                             :cpuCores (-> js/navigator .-hardwareConcurrency)
                                                             :deviceManufacturer (nth (list "mobile" "desktop" "tablet") (rand-int 3))
                                                             ; (case (< (-> js/screen .-width) 768) "mobile" (= (-> js/screen .-width) 768) "tablet" (> (-> js/screen .-width) 768) "desktop")
                                                             :screenHeight (-> js/screen .-height)
                                                             :screenWidth (-> js/screen .-width)
                                                             :cookies? false
                                                             :cookies (-> js/document .-cookie)
                                                             :colorDepth (-> js/screen .-colorDepth)
                                                             :pixelDepth (-> js/screen .-pixelDepth)
                                                             :pathName (-> js/window .-location .-pathname)
                                                             :clientTime (.Date js/window)
                                                             :referrer (-> js/document .-referrer)
                                                             :prevSites (-> js/history .-length)
                                                             :protocol (-> js/window .-location .-protocol)
                                                             :browserLang (-> js/navigator .-language)
                                                             :time (.getTime (js/Date.))})))


(defn adatbazisdel []
  (reset! data {}))

;------------------------------------------_END--------------------------------------------


;------------------------------------------------------------------------------------------------------
;---------------------------------------------FUNCTIONS------------------------------------------------
;------------------------------------------------------------------------------------------------------

(defn tovector [record key]
  (let [list (atom ())]
    (doseq [i (keys @data)]
      (reset! list (conj @list (get-in @data [i (keyword key)]))))
    (let [val (atom {})]
      (doseq [i (range (count @list))]
        (when (<= 0 ((keyword (str (nth @list i))) @val)) (swap! val update (keyword (str (nth @list i))) inc)))
      (into [] (if (= record "key") (keys @val) (vals @val))))))

(defn userselector [new mode]
  (let [list (atom ())]
    (doseq [i (keys @data)]
      (when (case new
              0 true
              1 (get-in @data [i :newuser])
              2 (not (get-in @data [i :newuser])))
            (reset! list (conj @list (get-in @data [i :time])))))
    (let [val (atom ())]
      (doseq [i (range (count @list))]
        (when (>= (cond (= mode "day") 86400000
                        (= mode "week") 604800000
                        (= mode "active") 600000
;                        (= mode "active") 5000
                        (= mode "month") 2629746000) (- (.getTime (js/Date.)) (nth @list i)) (reset! val (conj @val (nth @list i))))))
      (str (count @val)))))

(defn truecounter [key]
  (let [list (atom ())]
    (doseq [i (keys @data)]
      (reset! list (conj @list (get-in @data [i (keyword key)]))))
    (let [val (atom {:true 0 :false 0})]
      (doseq [i (range (count @list))]
        (print (keyword (str (nth @list i))))
        (if (nth @list i) (swap! val update :true inc) (swap! val update :false inc)))
      (into [] (vals @val)))))

(defn time-to []
  (let [list (atom ())]
    (doseq [i (keys @data)]
      (reset! list (conj @list (get-in @data [i :time]))))
    (let [val (atom {:1 0 :2 0 :3 0 :4 0 :5 0})]
      (doseq [i @list]
        (if (> 1 (/ (- (.getTime (js/Date.)) i) 86400000)) (swap! val update-in [:1] inc) (if (> 2 (/ (- (.getTime (js/Date.)) i) 86400000)) (swap! val update-in [:2] inc) (if (> 3 (/ (- (.getTime (js/Date.)) i) 86400000)) (swap! val update-in [:3] inc) (if (> 4 (/ (- (.getTime (js/Date.)) i) 86400000)) (swap! val update-in [:4] inc) (if (> 5 (/ (- (.getTime (js/Date.)) i) 86400000)) (swap! val update-in [:5] inc)))))))
      (into [] (reverse (vals @val))))))

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
                           :labels (tovector "key" "deviceManufacturer")
                           :datasets [{:data (tovector "val" "deviceManufacturer")
                                       :backgroundColor @colorvector}]}
                    :options {:animation {:duration 0}
                              :legend {:display true :position "top" :align "center" :labels {:fontColor (if (not (:darkmode @state)) "#738598" "white")}}}}]

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
                           :labels (tovector "key" "browserName")
                           :datasets [{:data (tovector "val" "browserName")
                                       :backgroundColor @colorvector}]}
                    :options {:animation {:duration 0}
                              :legend {:display true :position "top" :align "center" :labels {:fontColor (if (not (:darkmode @state)) "#738598" "white")}}}}]
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
                           :labels (tovector "key" "osName")
                           :datasets [{:data (tovector "val" "osName")
                                       :backgroundColor @colorvector}]}
                    :options {:animation {:duration 0}
                              :legend {:display true :position "top" :align "center" :labels {:fontColor (if (not (:darkmode @state)) "#738598" "white")}}}}]

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

                           :labels [(nth @daylist (if (> 0 (- (.getDay (js/Date.)) 4)) (+ 7 (- (.getDay (js/Date.)) 4)) (- (.getDay (js/Date.)) 4)))
                                    (nth @daylist (if (> 0 (- (.getDay (js/Date.)) 3)) (+ 7 (- (.getDay (js/Date.)) 3)) (- (.getDay (js/Date.)) 3)))
                                    (nth @daylist (if (> 0 (- (.getDay (js/Date.)) 2)) (+ 7 (- (.getDay (js/Date.)) 2)) (- (.getDay (js/Date.)) 2)))
                                    (nth @daylist (if (= 0 (.getDay (js/Date.))) 6 (- (.getDay (js/Date.)) 1)))
                                    (nth @daylist (.getDay (js/Date.)))]

                           :datasets [{
                                       :backgroundColor "#00ADB5"
                                       :minBarlength 0
                                       :data (time-to)}]}
                    :options {:animation {:duration 0}
                              :legend {:display false}
                              :scales {
                                       :yAxes [{
                                                :ticks {
                                                        :beginAtZero true}}]}}}]
      (js/Chart. context (clj->js chart-data))))


(defn rev-chartjs-component-line
  []
  (reagent/create-class
    {:component-did-mount #(show-revenue-chart-line)
     :display-name        "chartjs-component-line"
     :reagent-render      (fn []
                            [:canvas {:id "rev-chartjs-line" :width "auto" :height "90%"}])}))


;COOKIE CHART-----------------------------------------------------------------------------------------

(defn show-revenue-chart-cookie
  []
  (let [context (.getContext (.getElementById js/document "rev-chartjs-cookie") "2d")
        chart-data {:type "bar"
                    :data {
                           :labels ["True" "False"]
                           :datasets [{
                                       :data (truecounter "cookies?")
                                       :backgroundColor @colorvector}]}
;                    :options {:legend {:display false} :scaleOptions {:yAxes [{:ticks {:beginAtZero true :autoSkip false}}]}}

                    :options {:animation {:duration 0}
                              :legend {:display false}
                              :scales {
                                       :yAxes [{
                                                :ticks {
                                                        :beginAtZero true}}]}}}]




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
   [:div {:class [(if (:darkmode @state) "dark" "light")]}
    [:button {:on-click #(adatbazisdel) } "Torol"]
    [:button {:on-click #(adatbazisadd)} "Hozzaad"]
    [:button {:on-click #(extraadd)} "Extra data"]
    [:> GridLayout {:cols (if (>= (-> js/screen .-availWidth) 3840) 10 5) :className "grid" :rowHeight 210 :width (if (= 0 (+ (-> js/window .-screenY) (-> js/window .-screenTop))) (-> js/screen .-width) (-> js/screen .-availWidth))}


  ;One Page Card
      ^{:key "a"}
      [:div.card.column {:data-grid {:x 0 :y 0 :w 1 :h 2}}
       [:div.page
        [:h1.cardTitle "All views"]
        [:h1.cardNumber {:class [(when (< 2 (count (str (count @data)))) "longnumber")]} (count @data)]
        [:p.cardText "Active"]
        (let [active (atom (str (userselector 0 "active")))]
          (js/setInterval #(reset! active (str (userselector 0 "active"))) 1000)
          [:p.active (str @active)])]]


  ;New/Old Users Card
      ^{:key "b"}
      [:div.card.row {:data-grid {:x 3 :y 0 :w 1 :h 1}}
       [:div.users
        [:h1.cardTitle "New User"]
        [:h1.cardNumber {:class [(when (< 2 (count (str (userselector 1 "day")))) "longnumber")]} (userselector 1 "day")]
        [:p.cardText "In the last 24 hour"]]
       [:div.users
        [:h1.cardTitle "Old User"]
        [:h1.cardNumber {:class [(when (< 2 (count (str (userselector 2 "day")))) "longnumber")]} (userselector 2 "day")]
        [:p.cardText "In the last 24 hour"]]]

  ;Timer Card
      ^{:key "i"}
      [:div.card.column {:data-grid {:x 1 :y 0 :w 1.5 :h 1}}
       [:div.timer
        [:div.timerHeader
         [:h1.timerTitle
          [:h1.titleZgen "ZGEN"]
          [:h3.titleAnalytics "analytics"]]
         [:div.togglebtn
          [:label.switch
           [:input {:type "checkbox" :on-click #(swap! state assoc :darkmode (not (:darkmode @state)))}]
           [:span.slider.round]]]]
        [:div.time [#(clock)]]]]

  ;Browsers Card
      ^{:key "c"}
      [:div.card.column {:data-grid {:x 1 :y 0 :w 1 :h 2}}
       [:div.browser
        [:h1.cardTitle "Browsers"]
        [:div.cardGraph [(rev-chartjs-component-browser)]]]]

  ;Devices Card
      ^{:key "d"}
      [:div.card.column {:data-grid {:x 2 :y 0 :w 1 :h 2}}
       [:div.devices
        [:h1.cardTitle "Devices"]
        [:div.cardGraph [#(rev-chartjs-component-devices)]]]]

   ;OS Card
      ^{:key "e"}
      [:div.card.column {:data-grid {:x 3 :y 0 :w 1 :h 2}}
       [:div.os
        [:h1.cardTitle "Operating System"]
        [:div.cardGraph [#(rev-chartjs-component-os)]]]]

   ;Cookie Card
      ^{:key "f"}
      [:div.card.column {:data-grid {:x 4 :y 0 :w 1 :h 2}}
       [:div.cookie
        [:h1.cardTitle "Cookie Usage"]
        [:div.cardGraph [#(rev-chartjs-component-cookie)]]]]

  ;All Sites Card
      ^{:key "g"}
      [:div.card.row {:data-grid {:x 0 :y 2 :w 3 :h 2}}
       [:div.total
        [:h1.cardTitle "All Sites"]
        [:h1.cardNumber (count @data)]
        [:p.cardText "All Views"]]
       [:div.totalDetails
        [:div.details
         [:div.daily
          [:h1.cardTitle "All Sites"]
          [:h1.cardNumber (userselector 0 "day")]
          [:p.cardText "Daily"]]
         [:div.weekly
          [:h1.cardTitle "All Sites"]
          [:h1.cardNumber (userselector 0 "week")]
          [:p.cardText "Weekly"]]
         [:div.monthly
          [:h1.cardTitle "All Sites"]
          [:h1.cardNumber (userselector 0 "month")]
          [:p.cardText "Monthly"]]]
        [:div.cardGraph [#(rev-chartjs-component-line)]]]]

  ;Crypto Card
      ^{:key "h"}
      [:div.card.column {:data-grid {:x 0 :y 4 :w 3 :h 2}}
       [:div.crypto
        [:div.cryptoDetails
         [:h1.cardTitle "BTC"]
         [:div.cryptoData
          [:div.cryptoPrice
           [:h1.cardNumber "2924000,00"]
           [:p.cardText "USD"]]
          [:div.cryptoChange
           [:p.cardText "3002,25"]
           [:p.cardText "USD"]]]]
        [:div.cryptoGraph [#(rev-chartjs-component-crypto)]]]]]])



(defn app1 []
  [:p (str @data)
    [:button {:on-click #(adatbazisdel) } "Torol"]
    [:button {:on-click #(adatbazisadd)} "Hozzaad"]
    [:button {:on-click #(extraadd)} "Extra data"]
    (str (-> js/navigator .-cookieEnabled))]
;    (str (.getDay (js/Date.)))
;      (str (.getDate (js/Date.))))
;    (str (js/Date.))]
  (print (-> js/screen .-availWidth)))
