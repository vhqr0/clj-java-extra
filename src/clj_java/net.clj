(ns clj-java.net
  (:import [java.io InputStream OutputStream]
           [java.net InetAddress SocketAddress InetSocketAddress Socket URL URI]
           [javax.net.ssl SSLSocketFactory]))

(set! clojure.core/*warn-on-reflection* true)

;;; inet addr

(defprotocol IACoercions
  (^InetAddress as-ia [x]))

(extend-type nil
  IACoercions
  (as-ia [_]))

(extend-type InetAddress
  IACoercions
  (as-ia [a] a))

(extend-type InetSocketAddress
  IACoercions
  (as-ia [a] (.getAddress a)))

(extend-type String
  IACoercions
  (as-ia [s] (InetAddress/getByName s)))

(extend-type byte/1
  IACoercions
  (as-ia [b] (InetAddress/getByAddress b)))

;;; socket addr

(defprotocol SACoercions
  (^SocketAddress as-sa [x]))

(extend-type nil
  SACoercions
  (as-sa [_]))

(extend-type SocketAddress
  SACoercions
  (as-sa [a] a))

(extend-type Long
  SACoercions
  (as-sa [i] (InetSocketAddress. i)))

(extend-type clojure.lang.IPersistentVector
  SACoercions
  (as-sa [x]
    (let [[ia ^long port] x]
      (InetSocketAddress. (as-ia ia) port))))

;;; socket

(defprotocol SocketFactory
  (^Socket make-socket [x opts]))

(defn socket
  ^Socket [x & opts]
  (make-socket x (when opts (apply hash-map opts))))

(extend-type InetSocketAddress
  SocketFactory
  (make-socket [a opts]
    (let [{:keys [timeout ssl? ssl-host] :or {timeout 0}} opts
          socket (doto (Socket.) (.connect a timeout))]
      (if-not ssl?
        socket
        (let [^SSLSocketFactory factory (SSLSocketFactory/getDefault)
              ^String host (or ssl-host (.getHostName a))
              port (.getPort a)]
          (.createSocket factory socket host port true))))))

;;; url

(extend-type URL
  IACoercions
  (as-ia [url] (as-ia (.getHost url)))
  SACoercions
  (as-sa [url]
    (let [host (.getHost url)
          port (or
                (let [port (.getPort url)]
                  (when-not (= -1 port)
                    port))
                (.getDefaultPort url))]
      (as-sa [host port])))
  SocketFactory
  (make-socket [url opts]
    (let [ssl? (= "https" (.getProtocol url))]
      (make-socket (as-sa url) (merge {:ssl? ssl?} opts)))))

(extend-type URI
  IACoercions
  (as-ia [uri] (as-ia (.toURL uri)))
  SACoercions
  (as-sa [uri] (as-sa (.toURL uri)))
  SocketFactory
  (make-socket [uri opts] (make-socket (.toURL uri) opts)))

(comment
  (defn test-curl
    [url]
    (let [url (URL. url)
          host (.getHost url)
          req (format "GET / HTTP/1.1\r\nHost: %s\r\n\r\n" host)]
      (with-open [s (socket url)]
        (let [is (.getInputStream s)
              os (.getOutputStream s)
              fut (future (slurp is))]
          (.write os (.getBytes req))
          (.shutdownOutput s)
          @fut)))))
