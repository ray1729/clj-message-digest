(ns clj-message-digest.test.core
  (:use [clj-message-digest.core]
        [clojure.test])
  (:import [java.io File]))

(declare tmpfile)

(def test-string "foobar")
(def expected-md5sum-hex "3858f62230ac3c915f300c664312c63f")
(def expected-md5sum-base64 "OFj2IjCsPJFfMAxmQxLGPw")
(def expected-sha1sum-hex "8843d7f92416211de9ebb963ff4ce28125932878")
(def expected-sha1sum-base64 "iEPX+SQWIR3p67lj/0zigSWTKHg")

(defn tmpfile-fixture [f]
  (binding [tmpfile (doto (java.io.File/createTempFile "test" ".data")
                      .deleteOnExit)]
    (spit tmpfile test-string)
    (f)))

(use-fixtures :once tmpfile-fixture)

(deftest test-md5-string
  (testing "MD5 sum (hex) of a string"
    (is (= (md5-hex test-string) expected-md5sum-hex)))
  (testing "MD5 sum (base64) of a string"
    (is (= (md5-base64 test-string) expected-md5sum-base64))))

(deftest test-md5-file
  (testing "MD5 sum (hex) of a file"
    (is (= (md5-hex tmpfile) expected-md5sum-hex)))
  (testing "MD5 sum (base64) of a file"
    (is (= (md5-base64 tmpfile) expected-md5sum-base64))))

(deftest test-sha1-string
  (testing "SHA-1 sum (hex) of a string"
    (is (= (sha-1-hex test-string) expected-sha1sum-hex)))
  (testing "SHA-1 sum (base64) of a string"
    (is (= (sha-1-base64 test-string) expected-sha1sum-base64))))

(deftest tesh-sha1-file
  (testing "SHA-1 sum (hex) of a file"
    (is (= (sha-1-hex tmpfile) expected-sha1sum-hex)))
  (testing "SHA-1 sum (base64) of a file"
    (is (= (sha-1-base64 tmpfile) expected-sha1sum-base64))))
