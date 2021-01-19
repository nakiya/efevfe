(ns formula.events
  (:require
   [re-frame.core :as re-frame]
   [formula.db :as db]
   [formula.parser :as parser]
   [formula.subs :as subs]
   [clojure.string :as str]
   [instaparse.core :as insta]
   [day8.re-frame.http-fx]
   [ajax.core :as ajax]
   [formula.model :as model]
   [formula.localstorage :as ls]
   [cljs.reader :as reader]))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   (db/default-db 3 3)))

(re-frame/reg-event-db
 ::resize-db
 (fn [_ [_ rows cols]]
   (db/default-db rows cols)))

(defn lookup-cell [col row]
  (let [col (- (.charCodeAt col 0) 64)
        row (js/parseInt row)]
    ;;(println "lookup cell. col = " col ", row = " row ", type col = " (type col) ", type row = " (type row))
    @(re-frame/subscribe [::subs/cell-value row col])))

(def transform-map {:number #(js/parseInt %)
                    :add + :sub - :mult * :div /
                    :colid identity
                    :rowid identity
                    :cellref-1 lookup-cell
                    :cellrange parser/expand-range
                    :funcall parser/call-function
                    :prog identity})

(defn get-formula-value [formula]
  ;;(println formula)
  (insta/transform transform-map (parser/formula-grammar (subs formula 1))))

(defn set-db-cell-data [db cell data-value display-value]
  (assoc db :cell-data (model/set-values (db :cell-data) (cell "row") (cell "col") data-value display-value)))

(re-frame/reg-event-db
 ::update-cells
 (fn [db [_ cells]]
   (let [cell (first (js->clj cells))
         value (cell "value")
         ;;_ (println "::update-cells. cell, value = " cell value)
         ]
     (set-db-cell-data db cell value value))))

(re-frame/reg-event-fx
 ::calculate-grid
 (fn [{:keys [db]} [_]]
   ;;(println "sending calculate request with:" (model/model->js (:cell-data db)))
   {:db (-> db
         (assoc :btn-caption "Loading...")
         (assoc :error nil))
    :http-xhrio {:method          :post
                 :uri             "http://localhost:8080/evaluate"
                 :format          (ajax/json-request-format)
                 :params          {:grid (model/model->js (:cell-data db))}
                 :timeout         8000
                 :response-format (ajax/json-response-format)
                 :on-success      [::good-http-result]
                 :on-failure      [::bad-http-result]}}))

(re-frame/reg-event-db
 ::good-http-result
 (fn [db [_ result]]
   (if (result "error")
     (-> db
         (assoc :error (result "error"))
         (assoc :btn-caption "Evaluate!"))
     (let [new-grid (model/reset-grid-with-calc-results (:cell-data db) (result "grid"))]
       (-> db
           (assoc :cell-data new-grid)
           (assoc :btn-caption "Evaluate!"))))))

(re-frame/reg-event-db
 ::bad-http-result
 (fn [db [_ result]]
   (assoc db :btn-caption "Evaluate!")))

(re-frame/reg-event-fx
 ::set-row-count
 (fn [{:keys [db]} [_ value]]
   {:db db
    :fx [[:dispatch [::resize-db value (:col-count db)]]]}))

(re-frame/reg-event-fx
 ::set-col-count
 (fn [{:keys [db]} [_ value]]
   {:db db
    :fx [[:dispatch [::resize-db (:row-count db) value]]]}))

(re-frame/reg-event-db
 ::update-save-sheet-name
 (fn [db [_ sheet-name]]
   (assoc db :save-sheet-name sheet-name)))

(re-frame/reg-event-db
 ::save-sheet
 (fn [db _]
   (let [sheet-name (:save-sheet-name db)]
     (if (and (some? sheet-name) (not= sheet-name ""))
       (let [all-sheet-names (reader/read-string (ls/get-item "sheet-names"))]
         (ls/set-item! sheet-name (pr-str db))
         (ls/set-item! "sheet-names" (pr-str (if (nil? all-sheet-names)
                                               [sheet-name]
                                               (conj all-sheet-names sheet-name))))
         (js/alert (str "Sheet: " sheet-name " saved successfully")))
       (js/alert "Empty sheet name!")))))

(re-frame/reg-event-db
 ::update-load-sheet-name
 (fn [db [_ sheet-name]]
   (assoc db :load-sheet-name sheet-name)))

(re-frame/reg-event-db
 ::load-sheet
 (fn [db _]
   (let [sheet-name (:load-sheet-name db)]
     (println "::load-sheet : " sheet-name)
     (if (and (some? sheet-name) (not= sheet-name ""))
       (reader/read-string (ls/get-item sheet-name))))))