(ns formula.subs
  (:require
   [re-frame.core :as re-frame]
   [formula.localstorage :as ls]
   [cljs.reader :as reader]))

(re-frame/reg-sub
 ::name
 (fn [db]
   (:name db)))

(re-frame/reg-sub
 ::cell-data
 (fn [db]
   (:cell-data db)))

(re-frame/reg-sub
 ::cell-value
 (fn [db [_ row col]]
   (println "reg-sub row = " row ", col = " col)
   (let [val (get-in db [:cell-data row col :data-value])
         g (println "val = " val)] 
     (int val))))

(re-frame/reg-sub
 ::btn-caption
 (fn [db]
   (:btn-caption db)))

(re-frame/reg-sub
 ::error
 (fn [db]
   (:error db)))

(re-frame/reg-sub
 ::row-count
 (fn [db]
   (:row-count db)))

(re-frame/reg-sub
 ::col-count
 (fn [db]
   (:col-count db)))

(re-frame/reg-sub
 ::save-sheet-name
 (fn [db]
   (:save-sheet-name db)))

(re-frame/reg-sub
 ::all-sheet-names
 (fn [db]
   (let [sheets (reader/read-string (ls/get-item "sheet-names"))]
     sheets)))
