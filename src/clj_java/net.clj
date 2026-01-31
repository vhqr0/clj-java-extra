(ns clj-java.net
  (:import [java.net InetAddress SocketAddress InetSocketAddress Socket URL URI]))

(set! clojure.core/*warn-on-reflection* true)

(def byte-array-type (class (make-array Byte/TYPE 0)))

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

(extend-type SocketAddress
  SocketFactory
  (make-socket [a opts]
    (let [{:keys [timeout] :or {timeout 0}} opts]
      (doto (Socket.)
        (.connect a timeout)))))

;;; url

(extend-type URL
  IACoercions
  (as-ia [url] (as-ia (.getHost url)))
  SACoercions
  (as-sa [url] (as-sa [(.getHost url) (.getPort url)]))
  SocketFactory
  (make-socket [url opts] (make-socket (as-sa url) opts)))

(extend-type URI
  IACoercions
  (as-ia [uri] (as-ia (.toURL uri)))
  SACoercions
  (as-sa [uri] (as-sa (.toURL uri)))
  SocketFactory
  (make-socket [uri opts] (make-socket (.toURL uri) opts)))
