(ns assignment-2.prod
  (:require [assignment-2.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
