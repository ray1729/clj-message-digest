(ns clj-message-digest.core
  (:use [clojure.contrib.io :only (to-byte-array input-stream *byte-array-type*)]
        [clojure.string :only (lower-case)])
  (:require [clojure.contrib.base64 :as base64])
  (:import [java.security MessageDigest DigestInputStream]
           [java.io StringWriter InputStream File]))

(defn- digest->hex [digest]
  (apply str (map (partial format "%02x") digest)))

(defn- digest->base64 [digest]
  (let [output (StringWriter.)]
    (base64/encode (input-stream digest)
                   output
                   base64/*base64-alphabet*
                   nil)
    (.toString output)))

(defmulti digest (fn [algorithm obj] (class obj)))

(defmacro defdigest [algorithm]
  (let [n (lower-case algorithm)
        d (symbol n)
        d_hex (symbol (str n "-hex")) 
        d_base64 (symbol (str n "-base64"))]
    `(do
      (def ~d (partial digest ~algorithm))
      (def ~d_hex (comp digest->hex ~d))
      (def ~d_base64 (comp digest->base64 ~d)))))

(def algorithms ["MD2" "MD5" "SHA-1" "SHA-256" "SHA-384" "SHA-512"])

(doseq [a algorithms]
  (defdigest a))

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
