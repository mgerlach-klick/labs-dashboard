(ns dashboard.queries
  (:require [datascript.core :as d]))


;;---- QUERIES


(def ds-rules '[[(not-labs-project? ?p)
                 [?p :ProjectID ?pid]
                 [(not= ?pid 23409)]
                 [(not= ?pid 16897)]
                 [(not= ?pid 23405)]
                 [(not= ?pid 23404)]
                 [(not= ?pid 22295)]
                 ]])


;; (defn time-spent-on-projs
;;   "All projects for user with $name"
;;   [db name]
;;   (d/q '[:find ?p (sum ?duration) .
;;          :in $ ?name
;;          :with ?p
;;          :where
;;          [?l :person/name ?name]
;;          [?l :person/userid ?id]
;;          [?p :UserID ?id]
;;          [?p :ProjectName ?proj]
;;          [?p :Duration ?d]
;;          [(/ ?d 60) ?duration]
;;          ]
;;        db
;;        name))

(defn time-spent-on-projid
  "Time that user with uid $uid spent on project with $projid"
  [db projid uid]
  (or
   (d/q '[:find (sum ?duration) .
          :in $ ?uid ?projid
          :with ?p
          :where
          [?p :UserID ?uid]
          [?p :ProjectID ?projid]
          [?p :Duration ?mins]
          [(/ ?mins 60) ?duration]
          ]
        db
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
          :in $ ?uid
          :with ?p
          :where
          [?p :UserID ?uid]
          [?p :Duration ?mins]
          [(/ ?mins 60) ?duration]
          ]
        db
        uid)
   0))


;; Duration by project
(defn q [db query]
  (prn
   (d/q query
       db
        )))
