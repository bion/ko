(ns ko.util
  (:require [clojure.java.io :refer [input-stream]]))

(defn read-binary-file [file-path]
  (with-open [reader (input-stream file-path)]
    (let [length (.length (clojure.java.io/file file-path))
          buffer (byte-array length)]
      (.read reader buffer 0 length)
      (java.nio.ByteBuffer/wrap buffer))))
