(ns clj-java.crypto
  (:import [java.util HexFormat]
           [java.security SecureRandom MessageDigest Signature
            PublicKey PrivateKey KeyPair KeyPairGenerator]
           [java.security.spec AlgorithmParameterSpec RSAKeyGenParameterSpec ECGenParameterSpec]
           [javax.crypto Mac Cipher KeyAgreement]
           [javax.crypto.spec SecretKeySpec IvParameterSpec GCMParameterSpec]))

(set! clojure.core/*warn-on-reflection* true)

;;; key utils

(defn hex->bytes
  ^bytes [^String s]
  (let [hf (HexFormat/of)]
    (.parseHex hf s)))

(defn bytes->hex
  ^String [^bytes b]
  (let [hf (HexFormat/of)]
    (.formatHex hf b)))

(defn rand-bytes
  ^bytes [^long n]
  (let [b (byte-array n)]
    (-> (SecureRandom.) (.nextBytes b))
    b))

(defprotocol KeyCoercions
  (^SecretKeySpec as-key [x algo]))

(extend-type SecretKeySpec
  KeyCoercions
  (as-key [key _algo] key))

(extend-type byte/1
  KeyCoercions
  (as-key [key algo] (SecretKeySpec. key algo)))

;;; digest

;; algo: MD5 SHA1 SHA256

(defn digest
  ^bytes [^String algo ^bytes data]
  (-> (MessageDigest/getInstance algo)
      (.digest data)))

;;; mac

;; algo: HmacMD5 HmacSHA1 HmacSHA256

(defn mac
  ^bytes [^String algo key ^bytes data]
  (let [mac (doto (Mac/getInstance algo)
              (.init (as-key key algo)))]
    (.doFinal mac data)))

;;; cipher

;; algo: AES/CBC/PKCS5Padding AES/GCM/NoPadding ChaCha20-Poly1305

(defmulti ^AlgorithmParameterSpec as-cipher-param-by-algo
  (fn [_param algo] algo))

(defmethod as-cipher-param-by-algo :default [param _algo]
  (IvParameterSpec. param))

(defmethod as-cipher-param-by-algo "AES/GCM/NoPadding" [param _algo]
  (GCMParameterSpec. 128 param))

(defn as-cipher-param
  ^AlgorithmParameterSpec [param algo]
  (when (some? param)
    (if (instance? AlgorithmParameterSpec param)
      param
      (as-cipher-param-by-algo param algo))))

(defn crypt
  (^bytes [mode ^String algo key param ^bytes data]
   (crypt mode algo key param data nil))
  (^bytes [mode ^String algo key param ^bytes data ^bytes aad]
   (let [cipher (doto (Cipher/getInstance algo)
                  (.init (int mode)
                         (as-key key algo)
                         (as-cipher-param param algo)))]
     (when (some? aad)
       (.updateAAD cipher aad))
     (.doFinal cipher data))))

(def encrypt (partial crypt Cipher/ENCRYPT_MODE))
(def decrypt (partial crypt Cipher/DECRYPT_MODE))

;;; pubkey

;;;; kpg

;; algo: RSA EC X25519 Ed25519

(defmulti ^AlgorithmParameterSpec as-kpg-param-by-algo
  (fn [_param algo] algo))

(defmethod as-kpg-param-by-algo "RSA" [param _algo]
  (RSAKeyGenParameterSpec. param RSAKeyGenParameterSpec/F4))

(defmethod as-kpg-param-by-algo "EC" [param _algo]
  (ECGenParameterSpec. param))

(defn as-kpg-param
  ^AlgorithmParameterSpec [param algo]
  (when (some? param)
    (if (instance? AlgorithmParameterSpec param)
      param
      (as-kpg-param-by-algo param algo))))

(defn kpg
  (^KeyPair [^String algo]
   (kp-gen algo nil))
  (^KeyPair [^String algo param]
   (let [kpg (KeyPairGenerator/getInstance algo)]
     (when (some? param)
       (.initialize kpg (as-kpg-param param algo)))
     (.generateKeyPair kpg))))

;;;; ka

;; algo: ECDH X25519

(defn ka
  ^bytes [^String algo ^PrivateKey prikey ^PublicKey peer-pubkey]
  (let [ka (doto (KeyAgreement/getInstance algo)
             (.init prikey)
             (.doPhase peer-pubkey true))]
    (.generateSecret ka)))

;;;; sign

;; algo: RSASSA-PSS SHA256withRSA SHA256withECDSA EdDSA

(defn sign
  ^bytes [^String algo ^PrivateKey prikey ^bytes data]
  (let [signer (doto (Signature/getInstance algo)
                 (.initSign prikey)
                 (.update data))]
    (.sign signer)))

(defn verify
  ^Boolean [^String algo ^PublicKey pubkey ^bytes data ^bytes sign]
  (let [verifier (doto (Signature/getInstance algo)
                   (.initVerify pubkey)
                   (.update data))]
    (.verify verifier sign)))
