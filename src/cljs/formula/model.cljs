(ns formula.model
  (:require [clojure.string :as s]))

(defn col-disp->model [col]
  ;Skip row header column
  (- (.charCodeAt (first col)) 64))

(comment
  (col-disp->model "C")
  (col-disp->model "Z"))

(defn col-model->disp [col-index]
  (or (if (< col-index 1)
        (throw (js/Error (str "Invalid col-index: " col-index))))
      (.fromCharCode js/String (+ col-index 64))))

(comment
  (col-model->disp 2)
  (col-model->disp 0)
  (col-model->disp 1))

(defrecord Cell [data-value display-value row col readonly])
(defn make-cell [args]
  (into {} (map->Cell (merge {:readonly false :width 60} args))))

(comment
  (make-cell {:data-value 10 :display-value 10 :row 1 :col 2})
  (make-cell {:data-value 10 :display-value 10}))

(defn create-model [row-count col-count]
  (-> []
       ;First add column headers
      (conj
       (into [] (for [i (range (inc col-count))]
                  (if (= i 0)
                    (make-cell {:data-value "" :display-value "" :row 0 :col 0 :readonly true})
                    (make-cell {:data-value (col-model->disp i) :display-value (col-model->disp i) :row 0 :col i :readonly true})))))
      (into (for [i (range 1 (inc row-count))]
              (into []
                    (for [j (range (inc col-count))]
                      (if (= 0 j)
                             ;Add row header
                        (make-cell {:data-value i :display-value i :row i :col 0 :readonly true})
                        (make-cell {:data-value nil :display-value nil :row i :col j}))))))))

(comment
  (create-model 2 2)
  ,)

(defn set-display-value [model row col display-value]
  (assoc-in model [row col :display-value] display-value))

(defn set-values [model row col data-value display-value]
  ;;(println "set-values:" row col data-value display-value)
  ;;(println (type row) (type col))
  (-> model
      (assoc-in [row col :data-value] data-value)
      (assoc-in [row col :display-value] display-value)))

(defn coord-vec->ref [[row col]]
  (str (.fromCharCode js/String (+ col 65)) "." (inc row)))

(defn vect->map [v]
  (let [rows (count v)
        cols (count (first v))]
    (->>  (for [i (range rows) j (range cols)]
            [i j])
          (reduce #(assoc %1 (coord-vec->ref %2) (get-in v %2)) {}))))

(comment
  (vect->map [[1 2] [3 4]])
  
  ,)

(defn model->js [grid]
  (->> grid
       (drop 1)
       (map #(rest %))
       (map #(into [] %))
       (into [])
       (map (fn [v] (map #(:data-value %) v)))
       (map #(into [] %))
       (into [])
       (vect->map)
       (clj->js)))

(comment
  (model->js (create-model 2 2))
  
  ,)

(defn set-cellref-value [model cellref value]
  (let [col (col-disp->model (subs cellref 0 1))
        row (js/parseInt (subs cellref 2))]
    ;;(println "set-cellref-value:" cellref value)
    (set-display-value model row col value)))

(defn reset-grid-with-calc-results [model results]
  (let [new-grid (reduce (fn [model [cellref value]] (set-cellref-value model cellref value)) model results)]
    ;;(println "reset-grid-with-calc-results res: " new-grid)
    new-grid))