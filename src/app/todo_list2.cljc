(ns app.todo-list2
  (:require contrib.str
            #?(:clj [datascript.core :as d]) ; database on server
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            #?(:clj [next.jdbc :as jdbc])))

#?(:clj (defonce !conn (d/create-conn {}))) ; database on server
(e/def db) ; injected database ref; Electric defs are always dynamic

#?(:clj (defonce !pgconn (-> (jdbc/get-datasource {:dbtype   "postgres"
                                                   :dbname   "postgres"
                                                   :user     "postgres"
                                                   :password "postgres"})
                           (jdbc/get-connection))))

#?(:clj (defonce !state (atom 0)))

#?(:clj
   (defn execute-stmt! [conn stmt]
     (jdbc/execute! conn stmt)
     (swap! !state inc)))

(e/defn TodoItem [{:keys [db/id] :as e}]
  (e/server
    (let [status (:task/status e)]
      (e/client
        (dom/div
          (ui/checkbox
            (case status :active false, :done true)
            (e/fn [v]
              (e/server
                (execute-stmt! !pgconn ["UPDATE tasks SET active = ? WHERE id=?" (not v) id])
                (d/transact! !conn [{:db/id id
                                     :task/status (if v :done :active)}])
                nil))
            (dom/props {:id id}))
          (dom/label (dom/props {:for id}) (dom/text (e/server (:task/description e)))))))))

(e/defn InputSubmit [F]
  ; Custom input control using lower dom interface for Enter handling
  (dom/input (dom/props {:placeholder "Buy milk"})
             (dom/on "keydown" (e/fn [e]
                                 (when (= "Enter" (.-key e))
                                   (when-some [v (contrib.str/empty->nil (-> e .-target .-value))]
                                     (new F v)
                                     (set! (.-value dom/node) "")))))))

(defn to-datascript-format
  [{:keys [tasks/id tasks/description tasks/active]}]
  {:db/id            id
   :task/status      (if active :active :done)
   :task/description description})

(e/defn TodoCreate []
  (e/client
    (InputSubmit. (e/fn [v]
                    (e/server
                      (execute-stmt! !pgconn ["INSERT INTO tasks (description, active) VALUES (?,?)" v true])
                      (d/transact! !conn [{:task/description v
                                           :task/status :active}])
                      nil)))))

#?(:clj (defn todo-count [db]
          #_(count
              (d/q '[:find [?e ...] :in $ ?status
                     :where [?e :task/status ?status]] db :active))
          (:count
            (first (jdbc/execute! !pgconn ["SELECT COUNT(*) AS count FROM tasks WHERE active"])))))

#?(:clj (defn todo-records [db]
          #_(->> (d/q '[:find [(pull ?e [:db/id :task/description]) ...]
                        :where [?e :task/status]] db)
                 (sort-by :task/description))
          (->> (jdbc/execute! !pgconn ["SELECT * FROM tasks ORDER BY description"])
               (map to-datascript-format))))

(e/defn Todo-list []
  (e/server
    (binding [db (e/watch !state)]
      (e/client
        (dom/link (dom/props {:rel :stylesheet :href "/todo-list.css"}))
        (dom/h1 (dom/text "minimal todo list2"))
        (dom/p (dom/text "it's multiplayer, try two tabsies!"))
        (dom/div (dom/props {:class "todo-list"})
                 (TodoCreate.)
                 (dom/div {:class "todo-items"}
                          (e/server
                            (e/for-by :db/id [{:keys [db/id] :as e} (todo-records db)]
                                      (TodoItem. e))))
                 (dom/p (dom/props {:class "counter"})
                        (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count db))))
                        (dom/text " items left")))))))