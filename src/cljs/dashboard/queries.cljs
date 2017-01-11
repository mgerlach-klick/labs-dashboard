(ns dashboard.queries
  (:require [datascript.core :as d]))


;;---- QUERIES


(def ds-rules '[[(not-labs-project? ?p)
                 [?p :ProjectID ?pid]
                 [(not= ?pid 16897)]
                 [(not= ?pid 23405)]
                 [(not= ?pid 23404)]
                 [(not= ?pid 22295)]]

                [(in-reporting-period ?person ?entry)
                 [(get-else $ ?person :labs/from #inst "1900") ?from]
                 [(get-else $ ?person :labs/to #inst "2100") ?to]
                 [?entry :Date ?datetime]
                 [(>= ?datetime ?from)]
                 [(<= ?datetime ?to)]]
                ])



(defn time-spent-on-projid
  "Time that user with uid $uid spent on project with $projid"
  [db projid uid]
  (or
   (d/q '[:find (sum ?duration) .
          :in $ % ?uid ?projid
          :with ?p
          :where
          [?p :UserID ?uid]
          [?p :ProjectID ?projid]
          [?p :Duration ?mins]
          [(/ ?mins 60) ?duration]
          [?person :person/userid ?uid]
          (in-reporting-period ?person ?p)
          ]
        db
        ds-rules
        uid
        projid)
   0))



(defn time-spent-not-billable
  "Time that user with userid $uid spent on non-billable projects
  Defined as anything that gets billed to KlickInc (company ID 1) and isn't a specific lab project"
  [db uid]
  (or
   (d/q '[:find (sum ?duration) .
          :in $ % ?uid
          :with ?p
          :where
          [?p :UserID ?uid]
          [?p :Duration ?mins]
          [?p :ProjectID ?pid]
          [(/ ?mins 60) ?duration]
          ;; +labs-projects+
          [?p :CompanyID 1] ;Klick
          ;; [(= :nil ?nobill)]
          (not-labs-project? ?p)
          [?person :person/userid ?uid]
          (in-reporting-period ?person ?p)
          ]
        db
        ds-rules
        uid)
   0))

(defn time-spent-on-billable-projects
  "Time that user with userid $uid spent on billable non-labs projects.
  Defined as anything that doesn't get billed to KlickInc (company ID 1)"
  [db uid]
  (or
   (d/q '[:find (sum ?duration) .
          :in $ % ?uid
          :with ?p
          :where
          [?p :UserID ?uid]
          [?p :CompanyID ?cid]
          [(not= ?cid 1)] ;Klick
          [?p :Duration ?mins]
          [(/ ?mins 60) ?duration]
          [?person :person/userid ?uid]
          (in-reporting-period ?person ?p)
          ]
        db
        ds-rules
        uid)
   0))


(defn total-time-booked
  "time that user with id $uid booked overall"
  [db uid]
  (or
   (d/q '[:find (sum ?duration) .
          :in $ % ?uid
          :with ?p
          :where
          [?p :UserID ?uid]
          [?p :Duration ?mins]
          [(/ ?mins 60) ?duration]
          [?person :person/userid ?uid]
          (in-reporting-period ?person ?p)
          ]
        db
        ds-rules
        uid)
   0))


;; Duration by project
(defn q [db query]
  (prn
   (d/q query
       db
        )))
