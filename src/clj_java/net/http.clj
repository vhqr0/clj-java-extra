(ns clj-java.net.http
  (:import [java.util Arrays HashMap]
           [java.util.function BiPredicate]
           [java.time Duration]
           [java.io File InputStream]
           [java.net URI ProxySelector]
           [java.net.http
            HttpHeaders HttpClient HttpClient$Builder HttpClient$Version HttpClient$Redirect
            HttpRequest HttpRequest$Builder HttpRequest$BodyPublisher HttpRequest$BodyPublishers
            HttpResponse HttpResponse$BodyHandler HttpResponse$BodyHandlers]))

(set! clojure.core/*warn-on-reflection* true)

;;; client

(defn as-duration
  ^Duration [d]
  (if (instance? Duration d)
    d
    (Duration/ofMillis d)))

(def version-map
  {:http1.1 HttpClient$Version/HTTP_1_1
   :http2   HttpClient$Version/HTTP_2})

(defn as-version
  ^HttpClient$Version [v]
  (if (instance? HttpClient$Version v)
    v
    (get version-map v)))

(def redirect-map
  {:never  HttpClient$Redirect/NEVER
   :always HttpClient$Redirect/ALWAYS
   :normal HttpClient$Redirect/NORMAL})

(defn as-redirect
  ^HttpClient$Redirect [r]
  (if (instance? HttpClient$Redirect r)
    r
    (get redirect-map r)))

(def proxy-map
  {:no-proxy HttpClient$Builder/NO_PROXY})

(defn as-proxy
  ^ProxySelector [p]
  (if (instance? ProxySelector p)
    p
    (get proxy-map p)))

(defn ->client
  ^HttpClient [opts]
  (let [{:keys [connect-timeout version redirect proxy]} opts
        builder (cond-> (HttpClient/newBuilder)
                  (some? connect-timeout) (.connectTimeout (as-duration connect-timeout))
                  (some? version) (.version (as-version version))
                  (some? redirect) (.followRedirects (as-redirect redirect))
                  (some? proxy) (.proxy (as-proxy proxy)))]
    (.build builder)))

;;; headers

(def ^:private always-bipred
  (reify BiPredicate
    (^boolean test [this _ _] true)))

(defn map->headers
  (^HttpHeaders [headers]
   (map->headers headers always-bipred))
  (^HttpHeaders [headers ^BiPredicate bipred]
   (let [m (HashMap.)]
     (doseq [[k v] headers]
       (let [k (str k)
             v (map str (if (sequential? v) v [v]))
             v (Arrays/asList (into-array String v))]
         (.put m k v)))
     (HttpHeaders/of m bipred))))

(defn headers->map
  [^HttpHeaders headers]
  (->> (.map headers)
       (map
        (fn [[k v]]
          (let [v (vec v)
                v (if (= 1 (count v)) (first v) v)]
            [k v])))
       (into {})))

;;; request

(def method-map
  {:get    "GET"
   :post   "POST"
   :put    "PUT"
   :delete "DELETE"
   :head   "HEAD"})

(defn as-method
  ^String [m]
  (if (string? m)
    m
    (get method-map m)))

(defprotocol BodyCoercions
  (^HttpRequest$BodyPublisher as-body [x]))

(extend-type HttpRequest$BodyPublisher
  BodyCoercions
  (as-body [b] b))

(extend-type nil
  BodyCoercions
  (as-body [_] (HttpRequest$BodyPublishers/noBody)))

(extend-type String
  BodyCoercions
  (as-body [s] (HttpRequest$BodyPublishers/ofString s)))

(extend-type byte/1
  BodyCoercions
  (as-body [b] (HttpRequest$BodyPublishers/ofByteArray b)))

(extend-type File
  BodyCoercions
  (as-body [f] (HttpRequest$BodyPublishers/ofFile f)))

(extend-type InputStream
  BodyCoercions
  (as-body [is] (HttpRequest$BodyPublishers/ofInputStream is)))

(defn as-req-headers
  ^String/1 [headers]
  (->> headers
       (mapcat
        (fn [[k v]]
          (let [k (str k)
                v (map str (if (sequential? v) v [v]))]
            (->> v (mapcat #(vector k %))))))
       (into-array String)))

(defn ->request
  ^HttpRequest [opts]
  (let [{:keys [method url headers body timeout version] :or {method :get}} opts
        builder (-> (HttpRequest/newBuilder)
                    (.uri (URI/create url))
                    (.method (as-method method) (as-body body)))
        builder (cond-> builder
                  (some? headers) (.headers (as-req-headers headers))
                  (some? timeout) (.timeout (as-duration timeout))
                  (some? version) (.version (as-version version)))]
    (.build builder)))

;;; send

(defn as-body-handler
  ^HttpResponse$BodyHandler [handler]
  (cond
    (nil? handler) (HttpResponse$BodyHandlers/discarding)
    (instance? HttpResponse$BodyHandler handler) handler
    :else (case handler
            :byte-array (HttpResponse$BodyHandlers/ofByteArray)
            :string (HttpResponse$BodyHandlers/ofString)
            :input-stream (HttpResponse$BodyHandlers/ofInputStream))))

(defn request
  ([req]
   (request (->client req) req))
  ([^HttpClient client req]
   (let [{:keys [as]} req
         req (->request req)
         resp (.send client req (as-body-handler as))]
     {:body (.body resp)
      :headers (headers->map (.headers resp))})))
