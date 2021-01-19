(ns formula.db
  (:require [formula.model :as model]))

(defn default-db [row-count col-count]
  {:name "Sheet1"
   :row-count row-count
   :col-count col-count
   :btn-caption "Evaluate!"
   :error nil
   :save-sheet-name nil
   :load-sheet-name nil
   :cell-data (formula.model/create-model row-count col-count)})
