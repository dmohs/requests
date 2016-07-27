(ns dmohs.requests
  (:require
   [cljs.nodejs :as nodejs]
   clojure.string
   ))

(def buffer (nodejs/require "buffer"))
(def http (nodejs/require "http"))
(def https (nodejs/require "https"))
(def url (nodejs/require "url"))


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


(defn inbound? [ctx] (= :inbound (:type ctx)))


(defn- get-content [ctx]
  (let [k (if (inbound? ctx) :response :request)
        headers (get-in ctx [k :headers])
        body (get-in ctx [k :body])
        json? (= "application/json" (get-header-value :content-type headers))
        body (if (and json? body) (str (.stringify js/JSON (clj->js body) nil 2) "\n") body)
        headers (merge (if body {:content-length (.byteLength js/Buffer body "utf8")} nil) headers)]
    [headers body]))


(defn respond [ctx]
  (assert (= :inbound (:type ctx)))
  (let [res (:res ctx)
        [headers body] (get-content ctx)
        ctx (assoc-in ctx [:response :status-code]
                      (or (-> ctx :response :status-code) (if (nil? body) 204 200)))]
    (.writeHead res (-> ctx :response :status-code) (clj->js headers))
    (if body (.end res body) (.end res))
    (when-let [f @request-logger]
      (f (-> ctx (dissoc :req :res))))))


(defn- add-response [ctx res]
  (assoc ctx
         :res res
         :response {:headers (js->clj (.-headers res))
                    :http-version (.-httpVersion res)
                    :status-code (.-statusCode res)
                    :status-message (.-statusMessage res)}))


(defn send [ctx cb]
  (assert (= :outbound (:type ctx)))
  (let [[headers body] (get-content ctx)
        request (:request ctx)
        request (if (contains? request :url)
                  (let [parsed-url (.parse url (:url request))]
                    (merge {:hostname (.-hostname parsed-url)
                            :path (str (.-path parsed-url) (.-hash parsed-url))
                            :protocol (.-protocol parsed-url)}
                           (when-let [x (.-port parsed-url)] {:port x})
                           (dissoc request :url)))
                  request)
        library (if (or (= "https:" (:protocol request)) (= :https (:protocol request)))
                  https
                  http)
        r (.request library
                    (-> request (assoc :headers headers) (dissoc :body :protocol) clj->js)
                    (fn [res]
                      (let [ctx (add-response ctx res)]
                        (when-let [f @request-logger]
                          (f ctx))
                        (cb nil ctx))))]
    (.on r "error" (fn [err] (cb err nil)))
    (when body
      (.write r body))
    (.end r)
    ctx))


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


(defn create-context
  ([req res]
   {:id (find-clean-base64-uuid)
    :type :inbound
    :req req
    :request {:url (let [parsed (.parse url (.-url req) true)
                         sanitized (.parse js/JSON (.stringify js/JSON parsed))
                         converted (js->clj sanitized :keywordize-keys true)]
                     converted)
              :method (keyword (clojure.string/lower-case (.-method req)))
              :headers (reduce-kv
                        (fn [r k v]
                          (assoc r (keyword (clojure.string/lower-case k)) v))
                        {}
                        (js->clj (.-headers req)))
              :client-ip (last (clojure.string/split (.. req -connection -remoteAddress) #":"))}
    :res res
    :response {}})
  ([m]
   {:id (find-clean-base64-uuid)
    :type :outbound
    :request m}))


(defn status-code [ctx code]
  (assoc-in ctx [:response :status-code] code))


(defn create-cors-context [req res]
  (update-in (create-context req res) [:response :headers] assoc "Access-Control-Allow-Origin" "*"))


(defn json-body [ctx x]
  (let [location (if (inbound? ctx) :response :request)]
    (-> (if (inbound? ctx) ctx (assoc-in ctx [:request :method] :post))
        (update-in [location :headers] assoc :content-type "application/json")
        (assoc-in [location :body] x))))


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


(defn respond-with-not-found
  ([ctx] (respond-with-not-found ctx {:error :not-found :message "URL not found"}))
  ([ctx error]
   (respond (-> ctx (status-code 404) (json-body error)))))


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


(defn respond-with-bad-request [ctx error message]
  (-> ctx (status-code 400) (json-body {:error error :message message}) respond))


(defn send-server-error [ctx]
  (-> ctx (status-code 500) respond))


(defn- maybe-handle-url [fail? ctx url-regex method-set handler-fn & args]
  (let [request (:request ctx)]
    (if-let [matches (re-matches url-regex (get-in request [:url :pathname]))]
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
  (let [x (if (inbound? ctx) (:req ctx) (:res ctx))
        data (atom "")]
    (.on x "data" (fn [chunk] (swap! data str chunk)))
    (.on x "end" (fn [] (f (assoc-in ctx [(if (inbound? ctx) :request :response) :body] @data))))))


(defn json->clj [x]
  (try
    (js->clj (js/JSON.parse x))
    (catch js/Object e
      :json-parse-error)))


(defn collect-json-body [ctx f]
  (collect-body
   ctx
   (fn [ctx]
     (let [parsed (json->clj (:body (:request ctx)))]
       (if (= parsed :json-parse-error)
         (-> ctx
             (status-code 400)
             (json-body {:error :parse-failure :message "Failed to parse JSON from request"})
             respond)
         (f (assoc-in ctx [:request :body] parsed)))))))


(defn process-pipeline [ctx next-fn & rest-fns]
  (if rest-fns
    (next-fn ctx #(apply process-pipeline % rest-fns))
    (next-fn ctx)))
