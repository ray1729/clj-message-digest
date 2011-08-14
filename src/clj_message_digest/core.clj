(ns clj-message-digest.core
  (:use [clojure.contrib.io :only (to-byte-array input-stream *byte-array-type*)]
        [clojure.string :only (lower-case)])
  (:import [java.security MessageDigest DigestInputStream]
           [java.io StringWriter InputStream File]
           [org.apache.commons.codec.binary Base64]))

(defn- digest->hex [digest]
  (apply str (map (partial format "%02x") digest)))

(defn- digest->base64 [digest]
  (org.apache.commons.codec.binary.Base64/encodeBase64String digest))

(defmulti digest (fn [algorithm obj] (class obj)))

(defmacro define-digest [algorithm]
  (let [n (lower-case algorithm)
        d (symbol n)
        d_hex (symbol (str n "-hex")) 
        d_base64 (symbol (str n "-base64"))]
    `(do
      (def ~d (partial digest ~algorithm))
      (def ~d_hex (comp digest->hex ~d))
      (def ~d_base64 (comp digest->base64 ~d)))))

(def algorithms ["MD2" "MD5" "SHA-1" "SHA-256" "SHA-384" "SHA-512"])

(defmacro define-all-digests []
  `(do ~@(map (fn [n] `(define-digest ~n)) algorithms)))

(define-all-digests)

(defmethod digest *byte-array-type* [algorithm b]
  (.. (java.security.MessageDigest/getInstance algorithm)
      (digest b)))

(defmethod digest String [algorithm s]
  (digest algorithm (to-byte-array s)))

(defmethod digest java.io.InputStream [algorithm stream]
  (let [message-digest (java.security.MessageDigest/getInstance algorithm)
        digest-stream  (java.security.DigestInputStream. stream message-digest)
        bufsize        (* 1024 1024)
        buf            (byte-array bufsize)]
    (while (not= -1 (.read digest-stream buf 0 bufsize)))
    (.digest message-digest)))

(defmethod digest java.io.File [algorithm file]
  (digest algorithm (input-stream file)))
