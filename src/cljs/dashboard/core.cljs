(ns dashboard.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]
              [cljs-http.client :as http]
              [cljs.core.async :refer [<! put! take!]]
              [datascript.core :as d]
              [cljs.pprint :as pprint]
              [goog.string :as gstring]
              [goog.string.format]
              ))

(enable-console-print!)


(defonce conn (d/create-conn nil))



;;------- get data
(defn <time-entry
  ""
  [start end userids]
  (let [userids* (if (coll? userids) userids (vector userids))]
    (->  (str "https://genome.klick.com/api/TimeEntry.json"
              "?EndDate=" end "T23:59:59-04:00"
              "&StartDate=" start "T00:00:00-04:00"
              "&UserIDs=[" (apply str (interpose \,  userids*)) "]")
         (http/jsonp))))

(defn transact-time-entries! [db start end userids]
  (take! (<time-entry start end userids) (fn [data]
                                          (->> data
                                               :body
                                               :Entries
                                               (d/transact! db))
                                           (reagent/force-update-all))))


;; ----- hardcoded data
(def +labsters+ [
                 ["Yan" 5959]
                 ["Carolyn" 5123]
                 ["Andrew" 6157]
                 ["Ken" 4803]
                 ["Ani" 6308]
                 ["Max"  4966]
                 ["Kat" 5904]
                 ["Pete" 5466]
                 ["Stephen" 5417]
                 ])
(def labster-names (map first +labsters+))
(def labster-ids (map second +labsters+))

;;;     :IsClientBillable true, ?
(def +lab-projects+ {"Labs Billable" 23409
                     "Administration"  16897
                     "Studies" 23405
                     "Experiments" 23404
                     "Promo" 22295})

(def lab-project-ids (vals +lab-projects+))


;;----------- db stuff

(defn transact-users! [db users]
  (doseq [l users]
    (d/transact! db [{:person/name (first l)
                      :person/userid (second l)}]))
  (reagent/force-update-all))

(when (= @(d/create-conn nil) (d/empty-db))
  (transact-users! conn +labsters+ )
  (transact-time-entries! conn "2016-11-21" "2016-11-25" labster-ids)
  )

(defn format-hours [hrs]
  ;; (/ (Math/round (* 100 (/ min 60))) 100)
  (cond
    (zero? hrs) "-"
    (number? hrs) (.toFixed hrs 2)
    (string? hrs) hrs))


;;---- QUERIES

(defn time-spent-on-projs [name]
  (d/q '[:find ?p (sum ?duration) .
         :in $ ?name
         :with ?projid
         :where
         [?l :person/name ?name]
         [?l :person/userid ?id]
         [?p :UserID ?id]
         [?p :ProjectName ?proj]
         [?p :ProjectID ?projid]
         [?p :Duration ?d]
         [(/ ?d 60) ?duration]
         ]
       @conn
       name))

(defn time-spent-on-projid [projid uid]
  (or
   (d/q '[:find (sum ?duration) .
          :in $ ?uid ?projid
          :with ?projid
          :where
          [?p :UserID ?uid]
          [?p :ProjectID ?projid]
          [?p :Duration ?mins]
          [(/ ?mins 60) ?duration]
          ]
        @conn
        uid
        projid)
   0))

(defn time-spent-not-billable [uid]
  (or
   (d/q '[:find (sum ?duration) .
          :in $ ?uid
          :with ?pid
          :where
          [?p :UserID ?uid]
          [?p :IsClientBillable false]
          [?p :Duration ?mins]
          [?p :ProjectID ?pid]
          [(/ ?mins 60) ?duration]
          ;; +labs-projects+
          [(not= ?pid 23409)]
          [(not= ?pid 16897)]
          [(not= ?pid 23405)]
          [(not= ?pid 23404)]
          [(not= ?pid 22295)]
          ]
        @conn
        uid)
   0))

(defn time-spent-on-billable-projects [uid]
  (or
   (d/q '[:find (sum ?duration) .
          :in $ ?uid
          :with ?pid
          :where
          [?p :UserID ?uid]
          [?p :IsClientBillable true]
          [?p :Duration ?mins]
          [?p :ProjectID ?pid]
          [(/ ?mins 60) ?duration]
          ;; +labs-projects+
          [(not= ?pid 23409)]
          [(not= ?pid 16897)]
          [(not= ?pid 23405)]
          [(not= ?pid 23404)]
          [(not= ?pid 22295)]
          ]
        @conn
        uid)
   0))

(defn total-time-booked [uid]
  (or
   (d/q '[:find (sum ?duration) .
          :in $ ?uid
          :with ?pid
          :where
          [?p :UserID ?uid]
          [?p :Duration ?mins]
          [?p :ProjectID ?pid]
          [(/ ?mins 60) ?duration]
          ]
        @conn
        uid)
   0))


;; Duration by project
(defn q [query]
  (prn
   (d/q query
        @conn
        )))


;; -------------------------
;; Views

(defn home-page []
  [:div [:h2 "Welcome to dashboard"]
   [:div [:a {:href "/about"} "go to about page"]]])

(defn about-page []
  [:div [:h2 "About dashboard"]
   [:div [:a {:href "/"} "go to the home page"]]])

(defn current-page []
  [:div [(session/get :current-page)]])


(defn billing-matrix
  ""
  []
  (let [
        labs-billable (map (partial time-spent-on-projid (get +lab-projects+ "Labs Billable")) labster-ids)
        others-billable (map (partial time-spent-on-billable-projects) labster-ids)
        others-unbillable (map (partial time-spent-not-billable) labster-ids)
        administration (map (partial time-spent-on-projid (get +lab-projects+ "Administration")) labster-ids)
        experiments (map (partial time-spent-on-projid (get +lab-projects+ "Experiments")) labster-ids)
        studies (map (partial time-spent-on-projid (get +lab-projects+ "Studies")) labster-ids)
        promo (map (partial time-spent-on-projid (get +lab-projects+ "Promo")) labster-ids)
        personal-sum (map total-time-booked labster-ids)
        sum (partial reduce +)
        sum-all (reduce + personal-sum)
        percentall #(str (.toFixed (/ (* 100 (sum %)) sum-all) 2) \%)
        todo "todo"]
  `[
    ["-" ~@labster-names "SUM" "PCT"]
    ["Labs Billable" ~@labs-billable ~(sum labs-billable) ~(percentall labs-billable)]
    ["Others Billable" ~@others-billable ~(sum others-billable) ~(percentall others-billable)]
    ["Others NOT Billable" ~@others-unbillable ~(sum others-unbillable) ~(percentall others-unbillable)]
    ["Administration" ~@administration ~(sum administration) ~(percentall administration)]
    ["Experiments" ~@experiments ~(sum experiments) ~(percentall experiments)]
    ["Studies" ~@studies ~(sum studies) ~(percentall studies)]
    ["Promo" ~@promo ~(sum promo) ~(percentall promo)]
    ["SUM" ~@personal-sum ~sum-all "100% (hopefully)" ]
    ]))


(defn dashboard-page []
  (let [matrix (billing-matrix)
        matrix-row #(let [r (get matrix %)]
                      (vector :tr
                              (vector :th (first r))
                              (map (comp (partial vector :td)
                                         (partial format-hours))
                                   (rest (butlast r)))
                              (vector :td (last r))))]
    [:div
     [:table.table.table-hover.table-bordered ; {:class "table table-striped"}
      [:thead
       [:tr
        (map (partial vector :th) (get matrix 0))
       ]]

      [:tbody
       (matrix-row 1)
       (matrix-row 2)
       (matrix-row 3)
       (matrix-row 4)
       (matrix-row 5)
       (matrix-row 6)
       (matrix-row 7)
       (matrix-row 8)

       ]]]))

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'dashboard-page))

(secretary/defroute "/about" []
  (session/put! :about-page #'about-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
