(ns assignment-2.core
  (:require
   [reagent.core :as reagent]
   [reagent.dom :as rdom]
   [reagent.session :as session]
   [reitit.frontend :as reitit]
   [clerk.core :as clerk]
   [accountant.core :as accountant]
   [hazard.core :as hazard]
   [assignment-2.maze-generator :as maze-gen :refer [grid]]
   [assignment-2.maze-graphics :as maze-gfx]
   [assignment-2.maze-solver :as maze-slv]
   [assignment-2.maze-names :as maze-names :refer [name-list]]))

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
;; Page Utilities

(def algorithm (reagent/atom "Binary Tree"))
(def maze-size (reagent/atom 10))
(def mazes-generated (reagent/atom ""))
(def mazes (reagent/atom []))
(def maze-name (reagent/atom ""))

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
    (set! (.-onload reader) #(reset! mazes (:mazes (js->clj (js/JSON.parse (-> % .-target .-result)) :keywordize-keys true))))
    (.readAsText reader (aget (-> e .-target .-files) 0))))

(defn get-input-value [e]
  (-> e .-target .-value))

(defn save-maze []
  (let [id (count @mazes)
        type @algorithm]
    (do
      (swap! mazes conj {:id id :name (if (= @maze-name "") (first (hazard/words name-list 1)) @maze-name) :type type :size @maze-size :maze @grid})
      (reset! maze-name ""))))

(defn clear-saved-mazes []
  (do
    (reset! mazes [])
    (maze-gen/reset-grid @maze-size)))

(defn load-maze [e solve-coord maze-details]
  (let [maze-id (js/parseInt (.-value (aget (.-target e) 0)))
        {maze-name :name maze-type :type maze-size :size maze :maze} (get @mazes maze-id)]
    (do
      (.preventDefault e)
      (reset! grid maze)
      (reset! solve-coord {:start-x 0 :start-y 0 :end-x 0 :end-y 0})
      (reset! maze-details {:name maze-name :type maze-type :size maze-size}))))

;; -------------------------
;; Page components

(defn generator-menu []
  (fn []
    [:div.maze-menu
     [:div.maze-settings
      [:select {:on-change #(reset! algorithm (get-input-value %))}
       [:option {:value "Binary Tree" :selected (if (= @algorithm "Binary Tree") "selected") } "Binary Tree"]
       [:option {:value "Sidewinder" :selected (if (= @algorithm "Sidewinder") "selected")} "Sidewinder"]
       [:option {:value "Recursive Backtracker" :selected (if (= @algorithm "Recursive Backtracker") "selected")} "Recursive Backtracker"]]
      [:input {:type "number"
               :min "3"
               :max "100"
               :value @maze-size
               :on-change #(reset! maze-size (js/parseInt (get-input-value %)))}]]
     [:div.maze-saving
      [:input {:type "text" :placeholder "Enter Maze Name" :value (if (= @maze-name "") "" @maze-name) :on-change #(reset! maze-name (get-input-value %))}]
      [:input {:type "button" :value "Save Maze" :on-click #(save-maze)}]
      [:input {:type "button" :value "Clear Saved Mazes" :on-click #(clear-saved-mazes)}]
      [:input {:type "button" :value "Export Saved Mazes" :on-click #(download-object-as-json (clj->js {:mazes @mazes}) "mazes.json")}]]]))

(defn generator-page []
  (fn []
    [:main.main
     [:h1 "Maze Generator"]
     [generator-menu]
     [:p.maze-info [:b "Amount of Saved Mazes: "] (count @mazes)]
     [:div
      [:pre
       [:p.maze-display (do
                          (maze-gen/reset-grid @maze-size)
                          (maze-gfx/print-as-text (cond
                                                    (= @algorithm "Binary Tree") (maze-gen/carve-passages 0 "bt")
                                                    (= @algorithm "Sidewinder") (maze-gen/carve-passages 0 "sw")
                                                    (= @algorithm "Recursive Backtracker") (maze-gen/carve-passages (rand-int @maze-size) (rand-int @maze-size) "rb"))))]]]

     [:div.generate-button
      [:p [:b "Amount of Manual Generations: "] (count @mazes-generated)]
      [:input {:type "button" :value "Generate" :on-click #(swap! mazes-generated inc)}]]]))

(defn solver-menu [solve-coord maze-details]
  (fn []
    [:div.maze-menu
     (if (not-empty @mazes)
       [:form.maze-load {:on-submit #(load-maze % solve-coord maze-details)}
        [:select
         (for [item @mazes]
          ^{:key (:id item)} [:option {:value (:id item)} (:name item)])]
        [:input {:type "submit" :value "Load Maze"}]])
     [:div.maze-upload
      [:input {:type "file" :on-change #(handle-file-upload %)}]]]))

(defn solver-page []
  (let [solve-coord (reagent/atom {:start-x 0 :start-y 0 :end-x 0 :end-y 0})
        maze-details (reagent/atom {:name nil :type @algorithm :size (count @grid)})]
    (fn []
      [:main.main
       [:h1 "Maze Solver (A*)"]
       [solver-menu solve-coord maze-details]
       (if (some? (:name @maze-details))
         [:p.maze-info [:b "Maze Name: "] (:name @maze-details) " | " [:b "Maze Type: "] (:type @maze-details) " | " [:b "Maze Size: "] (:size @maze-details)]
         [:p.maze-info [:b "Maze Type: "] (:type @maze-details) " | " [:b "Maze Size: "] (:size @maze-details)])
       [:pre
        [:p.maze-display (maze-gfx/print-as-text (maze-slv/solve-grid (:start-x @solve-coord)
                                                                      (:start-y @solve-coord)
                                                                      (:end-x @solve-coord)
                                                                      (:end-y @solve-coord)
                                                                      @grid))]]
       [:div.solver-coords
        [:div
         [:label [:b "Start "]]
         [:input {:type "number" :value (:start-x @solve-coord) :min 0 :max (dec (count @grid)) :on-change #(swap! solve-coord assoc :start-x (js/parseInt (get-input-value %)))}]
         [:input {:type "number" :value (:start-y @solve-coord) :min 0 :max (dec (count @grid)) :on-change #(swap! solve-coord assoc :start-y (js/parseInt (get-input-value %)))}]]
        [:div
         [:label [:b "End "]]
         [:input {:type "number" :value (:end-x @solve-coord) :min 0 :max (dec (count @grid)) :on-change #(swap! solve-coord assoc :end-x (js/parseInt (get-input-value %)))}]
         [:input {:type "number" :value (:end-y @solve-coord) :min 0 :max (dec (count @grid)) :on-change #(swap! solve-coord assoc :end-y (js/parseInt (get-input-value %)))}]]]])))

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
       [page]])))

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
