(ns clj-mxf.bits
  (:use [clj-mxf.slingshot-workaround :only [try-assoc+]])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:gen-class))

(defn byte-to-uint8 [b]
  "Convert a possibly signed 8-bit integer to unsigned. This is needed because Java bytes are signed"
  (if (not (neg? b))
    b
    (+ 0x100 b)))

(defn byte-array?
  [coll]
  (= (type coll) (Class/forName "[B")))

(defn bytes-to-num [v]
  (when (> (count v) 8)
    (throw+ {:bytes-to-num-error true,
             :vector-too-long true}))
  (loop [v v,
         n 0]
    (if (= v [])
      n
      (recur
        (rest v)
        (bit-or (bit-shift-left n 8)
                (byte-to-uint8 (first v)))))))

(defn bytes-to-signed-num
  "treat input bytes as a signed number (in contrast with bytes-to-num, which always
   returns positive)"
  [v]
  (if (bit-test (first v) 7)
    (- (inc
            (bytes-to-num
              (map
                #(bit-and (bit-xor % 0xff) 0xff)
                v))))
    (bytes-to-num v)))    ; positive
