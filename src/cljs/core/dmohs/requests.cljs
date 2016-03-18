(ns dmohs.requests
  (:require
   [cljs.nodejs :as nodejs]
   clojure.string
   ))

(def buffer (nodejs/require "buffer"))


(defn time-call [k f & args]
  (.time js/console k)
  (let [r (apply f args)]
    (.timeEnd js/console k)
    r))


(def ^:private request-logger (atom nil))


(defn set-request-logger! [f]
  (reset! request-logger f))


(defn find-header [k headers]
  (if-let [v (get headers k)]
    [k v]
    (let [lower-case-key (clojure.string/lower-case (name k))]
      (some
       (fn [[k v]]
         (when (= (clojure.string/lower-case (name k)) lower-case-key)
           [k v]))
       headers))))


(defn get-header-value [k headers]
  (second (find-header k headers)))


(defn respond [ctx]
  (let [res (:res ctx)
        response (:response ctx)
        response-headers (:headers response)
        body (:body response)
        json? (= "application/json" (get-header-value :content-type response-headers))
        body (if json? (str (.stringify js/JSON (clj->js body) nil 2) "\n") body)
        status-code (or (:status-code response) (if (nil? body) 204 200))
        response-headers (merge (if body {:content-length (count body)} nil) response-headers)]
    (.writeHead res status-code (clj->js response-headers))
    (if body (.end res body) (.end res))
    (when-let [f @request-logger]
      (f (-> ctx
             (dissoc :req :res)
             (assoc-in [:response :status-code] status-code))))))


(defn- remove-base64-padding [x]
  (clojure.string/replace x "=" ""))


(defn- uuid->base64 [uuid]
  (remove-base64-padding
   (.toString
    (buffer.Buffer. (clojure.string/replace (str uuid) "-" "") "hex")
    "base64")))


(def ^:private unclean-regex #"[+/]")
(def ^:private reporting-threshold 10)


(defn- find-clean-base64-uuid []
  (or
   (some
    (fn [i]
      (let [s (uuid->base64 (random-uuid))]
        #_(.log js/console (str "[" i "]:" s))
        (if (re-find unclean-regex s)
          nil
          (do
            (when (>= i reporting-threshold)
              (.warn js/console (str "Found clean base64 UUID after " (inc i) " iterations.")))
            s))))
    (range 100))
   "cleanb64uuidnotfound00"))


(defn create-context [req res]
  {:id (find-clean-base64-uuid)
   :req req
   :request {:url (.-url req)
             :method (keyword (clojure.string/lower-case (.-method req)))
             :headers (reduce-kv
                       (fn [r k v]
                         (assoc r (keyword (clojure.string/lower-case k)) v))
                       {}
                       (js->clj (.-headers req)))
             :client-ip (last (clojure.string/split (.. req -connection -remoteAddress) #":"))}
   :res res
   :response {}})


(defn status-code [ctx code]
  (assoc-in ctx [:response :status-code] code))


(defn create-cors-context [req res]
  (update-in (create-context req res) [:response :headers] assoc "Access-Control-Allow-Origin" "*"))


(defn json-body [ctx x]
  (-> ctx
      (update-in [:response :headers] assoc :content-type "application/json")
      (assoc-in [:response :body] x)))


(defn add-cors-headers
  ([ctx]
   (add-cors-headers
    ctx
    #{:accept :accept-version :api-version :authorization :content-range :content-type
      :origin :range :x-requested-with}))
  ([ctx allowed-headers]
   (update-in ctx [:response :headers] merge
              {:access-control-allow-origin "*" :access-control-allow-credentials "true"
               :access-control-allow-headers
               (clojure.string/join "," (map name allowed-headers))
               :access-control-expose-headers
               (clojure.string/join "," (map name allowed-headers))
               :access-control-max-age 1728000})))


(defn respond-with-not-found [ctx]
  (respond (-> ctx (status-code 404) (json-body {:error :not-found :message "URL not found"}))))


(defn respond-with-method-not-allowed [ctx allowed-methods]
  (respond
   (-> ctx
       (status-code 405)
       (json-body {:error :method-not-allowed
                   :message (str "Method "
                                 (clojure.string/upper-case (name (:method (:request ctx))))
                                 " is not allowed on this URL. See the Allow header for"
                                 " allowed methods")})
       (update-in [:response :headers] assoc "Allow"
                  (clojure.string/join
                   ","
                   (map (comp clojure.string/upper-case name) allowed-methods))))))


(defn- maybe-handle-url [fail? ctx url-regex method-set handler-fn & args]
  (let [request (:request ctx)]
    (if-let [matches (re-matches url-regex (:url request))]
      (if (contains? method-set (:method request))
        (let [params (when-not (string? matches) (rest matches))
              ctx (if-not (empty? params) (assoc-in ctx [:request :url-params] params) ctx)]
          (apply handler-fn ctx args)
          nil)
        (if fail?
          (do
            (respond-with-method-not-allowed ctx method-set)
            nil)
          ctx))
      ctx)))


(defn handle-url [ctx url-regex method-set handler-fn & args]
  (apply maybe-handle-url true ctx url-regex method-set handler-fn args))


(defn handle-or-pass-url [ctx url-regex method-set handler-fn & args]
  (apply maybe-handle-url false ctx url-regex method-set handler-fn args))


(defn collect-body [ctx f]
  (let [req (:req ctx)
        data (atom "")]
    (.on req "data" (fn [chunk] (swap! data str chunk)))
    (.on req "end" (fn [] (f (assoc-in ctx [:request :body] @data))))))


(defn process-pipeline [ctx next-fn & rest-fns]
  (if rest-fns
    (next-fn ctx #(apply process-pipeline % rest-fns))
    (next-fn ctx)))
