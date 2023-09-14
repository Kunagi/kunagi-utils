(ns kunagi.utils.browser)

(defn url-params []
  (let [params (-> js/window.location.search js/URLSearchParams.)]
    (->> ^js params
         .keys
         (map str)
         (reduce (fn [m k]
                   (assoc m (keyword k)
                          (.get ^js params k)))
                 {}))))

(comment
  (url-params))

(defn url-param [k]
  (when k
    (-> js/window.location.search
        js/URLSearchParams.
        (.get (cond
                (keyword? k) (name k)
                (string? k) k
                :else (str k))))))

(defonce URL_PARAMS (atom (url-params)))

(let [push-state (.-pushState js/history)
      replace-state (.-replaceState js/history)]
  (set! (.-pushState js/history)
        (fn [target state title url]
          (let [push-state (.bind push-state js/history)]
            (push-state target state title url))
          (reset! URL_PARAMS (url-params))))
  (set! (.-replaceState js/history)
        (fn [state title url]
          (let [replace-state (.bind replace-state js/history)]
            (replace-state state title url)
            (reset! URL_PARAMS (url-params)))))
  (js/window.addEventListener "popstate" #(reset! URL_PARAMS (url-params))))
