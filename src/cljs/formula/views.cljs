(ns formula.views
  (:require
   [re-frame.core :as re-frame]
   [reagent.core :as r]
   [formula.subs :as subs]
   ["react-datasheet" :default ReactDataSheet]
   [oops.core :refer [oget]]))

(defn char-range [start end]
  (map str (range (int start) (inc (int end)))))

(defn create-dim-selection [values selected-value on-change]
  (->> values
       (map (fn [v]
              [:option (cond-> {:value v} 
                         (= v selected-value) (assoc :selected "selected")) v]))
       (into [:select {:on-change on-change}])))

(comment
  (create-dim-selection [2 3 4] 3 nil)
  ,)

(defn create-sheet-load-selection [available-sheets on-change]
  (->> available-sheets
       (map (fn [v] [:option v]))
       (into [:select {:on-change on-change :style {:margin "10px"}}])))

(defn main-panel []
  (let [name (re-frame/subscribe [::subs/name])
        data (re-frame/subscribe [::subs/cell-data])
        btn-caption (re-frame/subscribe [::subs/btn-caption])
        error (re-frame/subscribe [::subs/error])
        row-count (re-frame/subscribe [::subs/row-count])
        col-count (re-frame/subscribe [::subs/col-count])
        sheet-name (re-frame/subscribe [::subs/save-sheet-name])
        available-sheets (re-frame/subscribe [::subs/all-sheet-names])]
    [:div
     [:h2 @name]
     [:div
      [:div {:style {:display "inline-block" :margin "10px"}}
       [:p "Rows"]
       (create-dim-selection (range 2 20)
                         @row-count
                         #(re-frame/dispatch [:formula.events/set-row-count (js/parseInt (.. % -target -value))]))]
      [:div {:style {:display "inline-block"}}
       [:p "Columns"]
       (create-dim-selection (range 2 20)
                         @col-count
                         #(re-frame/dispatch [:formula.events/set-col-count (js/parseInt (.. % -target -value))]))]]
     [:button
      {:style {:margin "10px"}
       :on-click (fn [e]
                   (.preventDefault e)
                   (re-frame/dispatch [:formula.events/calculate-grid]))}
      @btn-caption]
     [:p {:style {:color "red", :font-size 10 :margin "10px"}} @error]
     [:div {:style {:margin "10px"}}
      [:> ReactDataSheet
       {:data @data
        :valueRenderer #(oget % :display-value)
        :dataRenderer #(oget % :data-value)
        :onCellsChanged #(re-frame/dispatch [:formula.events/update-cells %])}]]
     [:div {:style {:margin "10px"}}
      [:label "Save Sheet as:"]
      [:input {:type "text" :style {:margin "5px"}
               :on-blur #(re-frame/dispatch [:formula.events/update-save-sheet-name (.. % -target -value)])}]
      [:button {:on-click (fn [e] 
                            (.preventDefault e)
                            (re-frame/dispatch [:formula.events/save-sheet]))} "Save"]]
     [:div {:style {:margin "10px"}}
      [:label "Load Sheet:"]
      (create-sheet-load-selection @available-sheets 
                                   #(re-frame/dispatch [:formula.events/update-load-sheet-name (.. % -target -value)]))
      [:button {:on-click (fn [e]
                            (.preventDefault e)
                            (re-frame/dispatch [:formula.events/load-sheet]))} "Load"]]]))

