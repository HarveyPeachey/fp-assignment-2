(ns assignment-2.core
  (:require
   [reagent.core :as reagent]
   [reagent.dom :as rdom]
   [reagent.session :as session]
   [reitit.frontend :as reitit]
   [clerk.core :as clerk]
   [accountant.core :as accountant]
   [assignment-2.maze-generator :as maze-gen :refer [grid]]
   [assignment-2.maze-graphics :as maze-gfx]
   [assignment-2.maze-solver :as maze-slv]))

;; -------------------------
;; Routes

(def router
  (reitit/router
   [["/" :index]
    ["/solver" :solver]]))

(defn path-for [route & [params]]
  (if params
    (:path (reitit/match-by-name router route params))
    (:path (reitit/match-by-name router route))))

;; -------------------------
;; Page components

(defn to-json [v] (.stringify js/JSON v))

(defn download-object-as-json [value export-name]
 (let [data-blob (js/Blob. #js [(to-json value)] #js {:type "application/json"})
       link (.createElement js/document "a")]
   (set! (.-href link) (.createObjectURL js/URL data-blob))
   (.setAttribute link "download" export-name)
   (.appendChild (.-body js/document) link)
   (.click link)
   (.removeChild (.-body js/document) link)
   (.revokeObjectURL js/URL data-blob)))

(defn handle-file-upload [e]
  (let [reader (js/FileReader.)]
    (set! (.-onload reader) #(js/console.log (-> % .-target .-result)))
    (.readAsText reader (aget (-> e .-target .-files) 0))))

(def is-bt? (reagent/atom true))
(def maze-size (reagent/atom 10))
(def mazes-generated (reagent/atom ""))
(def mazes (reagent/atom []))

(defn save-maze []
  (let [id (count @mazes)
        type (if is-bt?
               "Binary Tree"
               "Recursive Backtracker")]
    (swap! mazes conj {:id id :name "Cool Maze" :type type :size @maze-size :maze @grid})))

(defn clear-saved-mazes []
  (do
    (reset! mazes [])
    (maze-gen/reset-grid @maze-size)))

(defn generator-config []
  (fn []
    [:div
     [:select {:on-change #(swap! is-bt? not)}
      [:option {:value "bt"} "Binary Tree"]
      [:option {:value "rb" :selected (if (not @is-bt?) "selected")} "Recursive Backtracker"]]
     [:input {:type "number"
              :min "3"
              :max "100"
              :value @maze-size
              :on-change #(reset! maze-size (js/parseInt (-> % .-target .-value)))}]
     [:input {:type "text" :placeholder "Enter Maze Name"}]
     [:input {:type "button" :value "Save Maze" :on-click #(save-maze)}]
     [:input {:type "button" :value "Clear Mazes" :on-click #(clear-saved-mazes)}]
     [:input {:type "button" :value "Export Mazes" :on-click #(download-object-as-json (clj->js  @mazes) "mazes.json")}]
     [:div "Amount of saved mazes: " (count @mazes)]]))

(defn generator-page []
  (fn []
    [:span.main
     [:h1 "Maze Generator" " "]
     [generator-config]
     (if (= @is-bt? true)
       [:div
        [:p "Binary Tree Maze"]
        [:pre
         [:p {:style {:line-height "normal"
                      :zoom "0.8"}} (do
                                      (maze-gen/reset-grid @maze-size)
                                      (maze-gfx/print-as-text (maze-gen/carve-passages)))]]]
       [:div
        [:p "Recursive Backtracker Maze"]
        [:pre
         [:p {:style {:line-height "normal"
                      :zoom "0.8"}} (do
                                      (maze-gen/reset-grid @maze-size)
                                      (maze-gfx/print-as-text (maze-gen/carve-passages 0 0 "rb")))]]])
     [:div "Amount of Manual Generations: " (count @mazes-generated)]
     [:input {:type "button" :value "Generate" :on-click #(swap! mazes-generated inc)}]]))


(defn solver-page []
  (let [solve-coord (reagent/atom {:start-x 0 :start-y 0 :end-x 0 :end-y 0})]
    (fn []
      [:span.main
       [:h1 "Maze Solver"]
       [:select
        (for [item @mazes]
         ^{:key (:id item)} [:option {:value (:id item)} (:name item)])]
       [:input {:type "button" :value "Load Maze"}]
       [:input {:type "file" :on-change #(handle-file-upload %)}]
       (if (= @is-bt? true)
         [:p "Binary Tree Maze"]
         [:p "Recursive Backtracker Maze"])
       [:pre
        [:p {:style {:line-height "normal" :zoom "0.8"}} (maze-gfx/print-as-text (maze-slv/solve-grid (:start-x @solve-coord)
                                                                                                      (:start-y @solve-coord)
                                                                                                      (:end-x @solve-coord)
                                                                                                      (:end-y @solve-coord)
                                                                                                      @grid))]]
       [:input {:type "number" :value (:start-x @solve-coord) :min 0 :max (dec (count @grid)) :on-change #(swap! solve-coord assoc :start-x (js/parseInt (-> % .-target .-value)))}]
       [:input {:type "number" :value (:start-y @solve-coord) :min 0 :max (dec (count @grid)) :on-change #(swap! solve-coord assoc :start-y (js/parseInt (-> % .-target .-value)))}]
       [:input {:type "number" :value (:end-x @solve-coord) :min 0 :max (dec (count @grid)) :on-change #(swap! solve-coord assoc :end-x (js/parseInt (-> % .-target .-value)))}]
       [:input {:type "number" :value (:end-y @solve-coord) :min 0 :max (dec (count @grid)) :on-change #(swap! solve-coord assoc :end-y (js/parseInt (-> % .-target .-value)))}]])))

;; -------------------------
;; Translate routes -> page components

(defn page-for [route]
  (case route
    :index #'generator-page
    :solver #'solver-page))


;; -------------------------
;; Page mounting component

(defn current-page []
  (fn []
    (let [page (:current-page (session/get :route))]
      [:div
       [:header
        [:p [:a {:href (path-for :index)} "Maze Generator"] " | "
         [:a {:href (path-for :solver)} "Maze Solver"]]]
       [page]
       [:footer
        [:p "assignment-2 was generated by the "
         [:a {:href "https://github.com/reagent-project/reagent-template"} "Reagent Template"] "."]]])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (rdom/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (clerk/initialize!)
  (accountant/configure-navigation!
   {:nav-handler
    (fn [path]
      (let [match (reitit/match-by-path router path)
            current-page (:name (:data  match))
            route-params (:path-params match)]
        (reagent/after-render clerk/after-render!)
        (session/put! :route {:current-page (page-for current-page)
                              :route-params route-params})
        (clerk/navigate-page! path)))

    :path-exists?
    (fn [path]
      (boolean (reitit/match-by-path router path)))})
  (accountant/dispatch-current!)
  (mount-root))
