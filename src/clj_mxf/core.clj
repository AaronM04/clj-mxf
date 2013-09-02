(ns clj-mxf.core
  (:use [clj-mxf.slingshot-workaround :only [try-assoc+]])
  (:use clj-mxf.ul)
  (:use clj-mxf.bits)
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:use [clojure.pprint :only [pprint]])
  (:require [clojure.walk :as w])
 ;(:require [clojure.zip  :as z])
  (:import java.io.RandomAccessFile) ; http://docs.oracle.com/javase/1.4.2/docs/api/java/io/RandomAccessFile.html
  (:gen-class))

(defn asub
  "Create a vector out of part of an array. If :positive true, then ensures that all numbers
   are positive."
  ([^bytes arr start size & {:keys [positive]}]
    (let [coll
           (for [i (range start (+ start size))]
             (aget arr i))]
      (vec (if positive
             (map byte-to-uint8 coll)
             coll)))))

(defn sub-array
  "Like asub, except create a new byte array from the old"
  [^bytes arr start size]
  (let [result (byte-array size)]
    (loop [src start, dest 0]
      (aset result dest (aget arr src))
      (if (< (inc dest) size)
        (recur (inc src) (inc dest))
        result))))

(defn parse-ber-length
  "Returns [length bytes-consumed]"
  [v]
  (let [v (subvec v 0 9),
        b (byte-to-uint8 (first v))]
    (if (bit-test b 7)
      (let [consumed (inc (bit-and b 0x0f))]
        (if (<= consumed (min 9 (count v)))
          [(bytes-to-num (subvec v 1 consumed)) consumed]
          :XXX   ; TODO: throw exception
        ))
      [b 1])))

(defn bytes-to-utf16str [coll]
  (String.
    (if (byte-array? coll)
      coll
      (into-array Byte/TYPE coll))
    "UTF-16"))

(defn utf16str-to-bytes
  "The returned vector never has the BOM at the start.  It must be added to the
   front if it is needed"
  [s]
  (let [v (vec (.getBytes s "UTF-16"))]
    (if (or
          (< (count v) 2)
          (and
            (not (= [-2 -1] (subvec v 0 2)))     ; big-endian
            (not (= [-1 -2] (subvec v 0 2)))))   ; little-endian
      v
      (subvec v 2))))   ; take off the BOM

(declare parse-tag-data)

(defn parse-array
  "Parses an array. In most cases, arrays are present inside local sets, but a
   PrimerPack has an array as its KLV payload.
   
   The first 4 bytes is the element count, followed by a 4-byte element size,
   and finally the elements"
  [v typ]
  (let [el-count (bytes-to-num (subvec v 0 4)),   ; not used; TODO: consistency check?
        el-size  (bytes-to-num (subvec v 4 8))]
    (vary-meta
      (vec
        (for [i (range 8 (count v) el-size)]
          (parse-tag-data (subvec v i (+ i el-size)) typ))) ; throws IndexOutOfBoundsException
      assoc :type     :array
            :el-count el-count
            :el-size  el-size
            :el-type  typ)))

;:id, :date, :utf16str, [:array <type-or-size>], :unk, :ref, :ul, :umid, :rational, :uint8, :uint16, :uint32, :boolean, :int8, :int16, :int32, :uint64, [:batch <type-or-size>], :rgbalayout, :ppentry
(defn parse-tag-data
  "Given a byte vector and type (:utf16str, [:array :ref], etc.), return an object
   representing that type, or nil if not recognized"
  [v typ] 
  (if (vector? typ)
    (let [typ' (second typ)]
      (cond
        (or
          (= :batch (first typ))            ; a batch is an unordered array; MXF book pg. 90
          (= :array (first typ))) (try
                                    (parse-array v typ')
                                    (catch IndexOutOfBoundsException e
                                      (vary-meta v assoc :array-fail true)))))
    (let [handler (get {

                ;:type (fn [v] :XXXhandleit),
                :boolean    (fn [v] (not (zero? (first v)))),
                :date       (fn [v] [:Year    (bytes-to-num (subvec v 0 2)),
                                     :Month   (byte-to-uint8 (nth v 2)),
                                     :Day     (byte-to-uint8 (nth v 3)),
                                     :Hour    (byte-to-uint8 (nth v 4)),
                                     :Min     (byte-to-uint8 (nth v 5)),
                                     :Sec     (byte-to-uint8 (nth v 6)),
                                     :QMSec   (byte-to-uint8 (nth v 7)),  ; 1/250th of a sec
                                    ]),
                :int8       (fn [v] (bytes-to-signed-num v)),
                :int16      (fn [v] (bytes-to-signed-num v)),
                :int32      (fn [v] (bytes-to-signed-num v)),
                :int64      (fn [v] (bytes-to-signed-num v)),
                :ppentry    (fn [v] [(bytes-to-num (subvec v 0 2))
                                     (parse-tag-data (subvec v 2) :ul)]), ; PrimerPack entry
                :rational   (fn [v] (/ (bytes-to-signed-num (subvec v 0 4))    ; numerator
                                       (bytes-to-signed-num (subvec v 4 8)))), ; denominator
                :ref        (fn [v] v),
                :rgbalayout (fn [v] [:Code  (byte-to-uint8 (first v)),
                                     :Depth (byte-to-uint8 (second v))]),
                :uint8      (fn [v] (bytes-to-num v)),
                :uint16     (fn [v] (bytes-to-num v)),
                :uint32     (fn [v] (bytes-to-num v)),
                :uint64     (fn [v] (bytes-to-num v)),
                :ul         (fn [v] (if-let [ulkey (to-ulkey (vec (map byte-to-uint8 v)))]
                                      ulkey
                                      v)),
                :umid       (fn [v] v),
                :version    (fn [v] [:Major   (bytes-to-num (subvec v 0  2)),
                                     :Minor   (bytes-to-num (subvec v 2  4)),
                                     :Patch   (bytes-to-num (subvec v 4  6)),
                                     :Build   (bytes-to-num (subvec v 6  8)),
                                     :Release (bytes-to-num (subvec v 8 10))]),
                :utf16str   (fn [v] (bytes-to-utf16str v)),
                :unk        (fn [v] v),

                       } typ)] 
      (if (nil? handler)
        nil
        (let  [result (try-assoc+
                        (handler v)
                        (rethrow-assoc
                          :parse-tag-data-error true,
                          :tag-type typ,
                          :tag-data-vector v))]
          (try
            (vary-meta result assoc :type typ)
            (catch ClassCastException e
              result)))))))           ; some types don't support metadata

(defn parse-local-set
  "Parse the payloads of KLVs with local set coding; the tag is nil and the payload is
   :incomplete if there was an index-out-of-bounds exception"
  ([arr offset size & previous-tags]
    (let [four         (asub arr offset 4),
          [tagkey typ] (lookup-tag (bytes-to-num (subvec four 0 2)))
          tsize        (bytes-to-num (subvec four 2 4))]  ; size of payload
        (let [current-tag 
               (try-assoc+
                 (if (nil? tagkey)
                   [(->> (subvec four 0 2)
                          bytes-to-num
                         (format "%04x"))
                    (sub-array arr (+ offset 4) tsize)] ; not found
                   [tagkey (parse-tag-data (asub arr (+ offset 4) tsize) typ)])  ; found
                 (rethrow-assoc
                   :parse-local-set-error true,
                   :tag-key (if nil? tagkey (->> (subvec four 0 2) bytes-to-num)),
                   :tag-size tsize,
                   :tag-offset-in-klv offset))
              next-size (- size tsize 4),
              next-tags (conj (vec previous-tags) current-tag)]
          (if (pos? next-size)
            (recur arr (+ offset tsize 4) next-size next-tags)
            next-tags))))

  ([v] :XXX))

(defn parse-primer-pack
  "Parses the payload of a PrimerPack KLV. This consists of an array of 18 byte
   elements. Each element is a 2-byte tag, and a 16-byte UL which that tag is the
   shorthand for. NOTE: normally, arrays only occur inside a local set."
  [arr]
  (parse-array (vec arr) :ppentry))

(defn avid-id-index?
  "Heuristic for detecting the Avid ID index; there may be false positives, but
   there shouldn't be any false negatives"
  [arr]
  (and (>= (count arr) 9)
       (== (nth arr 8) 0x19)
       (=  (asub arr 0 4) [0 0 0 0])  ; this assumes there are less than 2^32 entries
       (zero? (mod (- (count arr) 9) 0x19))))

(defn parse-avid-id-index
  "The last KLV in the header for an Avid MXF. The key is not a UL (Doesn't
   start with 060e...). Each element is 25 bytes, and contains a 16-byte ID which
   points to the InstanceUID of a previously seen KLV, followed by an uint64 which
   is the file offset of the KLV with this InstanceUID, followed by a 00 byte
   with unknown purpose.

  At the beginning is an uint64 for the number of elements, and then a byte (or
  possibly a BER length?) indicating the size of each element (0x19 = 25 bytes)"
  [arr]
  (let [parse-element
         (fn [el]
           (let [[id off256] (split-at 16 el)]
             [(vec id) (-> off256
                           drop-last     ; remove the 00 byte at the end
                           bytes-to-num)]))]
    (vary-meta
      (->> arr
           (drop 9)
           (partition 25)
           (map parse-element)
           vec)
      assoc :avid-id-index true)
  )
)


(defn quick-scan
  "Given a RandomAccessFile, quickly scans a file, returning a vector of maps
   like {:offset 0x1234, :data-start 0x124d, :data-size 0x4567}. If not a map, then
   there was an anomalous condition (:eof for premature EOF, and others to be defined)."
  ; TODO: make parse-klvs use this; much cleaner
  [f]
  (let [b (byte-array 0x19),
        len (.length f)]
    (.seek f 0)
    ; TODO: handle run-in

    (loop [offset 0,
           items []]
      (if (< offset len)
        (do
          (.seek f offset)
          (let [n (.read f b),
                [data-size consumed] (if (>= n 0x11)
                                       (parse-ber-length (asub b 0x10 9))
                                       nil),
                data-start (if (nil? consumed)
                             nil
                             (+ offset 0x10 consumed))]
            (if (or (nil? data-size)
                    (> (+ 0x10 consumed) n))
              (conj items :eof)
              (recur (+ data-start data-size)
                     (conj items {:offset     offset,
                                  :data-start data-start,
                                  :data-size  data-size})))))
        (if (> offset len)
          (conj items :eof)
          items)))))


(defn parse-klv
  "Given either a byte array with offset, or a vector of bytes, return
   [[K, L, V], consumed] where L is a BigInt and K and V are vectors of bytes
   if unrecognized, or non-vectors if recognized.

   If called with :raw-payload true, the payload will be a byte array.
   If a number is supplied for :max-payload, then payloads above this byte size
   will be translated to :payload-too-big."
  ([arr offset & {:keys [raw-payload, max-payload]}]
    (let [ul         (asub arr offset 16 :positive true),
          ulkey      (to-ulkey ul),
          [length c] (parse-ber-length (asub arr
                                         (+ offset 16)
                                         17)),
          data-start (+ offset 16 c),
          payload    (try (sub-array arr data-start length)
                       (catch ArrayIndexOutOfBoundsException e
                         (prn [data-start length]))),
          consumed   (+ 16 c length)]

      (let [expected (+ data-start length),
            actual   (count arr)]
        (when (> expected actual)
          (throw+ {:parse-klv-error true,
                   :klv-offset offset,
                   :not-enough-data true,
                   :expected-bytes expected,
                   :actual-bytes actual})))
      ;(binding [*out* *err*] (printf "DEBUG: parsing %s (%s)\n" (str ulkey) (to-hex ul)))
      (try-assoc+
        [(vary-meta
          [(if (nil? ulkey)
             ul
             ulkey)
           length
           (cond
             (and (number? max-payload)
                  (> (count payload) max-payload))  :payload-too-big
             raw-payload   payload
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             (= ulkey :PrimerPack)
               (parse-primer-pack payload)
             (and (not= UL_PREFIX (subvec ul 0 4))
                  (avid-id-index? payload))
               (parse-avid-id-index payload)
             (or (= (subvec ul 4 7) [2 0x53 1])       ; MXF book pg. 77
                 (= (subvec ul 0 4) [-128 0x53 8 0])) ; it's an Avid thing
               (parse-local-set payload 0 (count payload))
             (= ulkey :UnkXDCAM_XML)
               (String. payload "UTF-8")  ; XML metadata in XDCAM files
             :else payload          ; byte array
           )]
          assoc :offset offset, :data-start data-start),
       consumed]
      (rethrow-assoc
        :parse-klv-error true,
        :klv-offset offset,
        :klv-ul ul)
    )))
  ([v] :XXX ))


(defn parse-klvs
  "Returns a vector [klvs, consumed], where klvs is a vector of all KLVs found
   by parse-klv, and consumed is the number of bytes consumed.
   
   If called with :raw-payload true, then the payloads will be byte arrays.
   If a number is supplied for :max-payload, then payloads above this byte size
   will be translated to :payload-too-big. The default is 30000."
  [arr offset & {:keys [raw-payload, max-payload]
                 :or {max-payload 30000}}]         ; I haven't seen metadata KLV bigger than this
  (loop [offset offset,
         consumed 0,
         klvs []]
    (let [[klv c] (try (parse-klv arr offset :raw-payload raw-payload,
                                             :max-payload max-payload)
                    (catch IndexOutOfBoundsException e
                      (throw+ {:parse-klv-error true,
                               :array-size (count arr),
                               :offset offset,
                               :klvs-already-read (count klvs)})))]
      (if (>= (+ offset c) (count arr))
        [(conj klvs klv) consumed]
        (recur
          (+ offset c)
          (+ consumed c)
          (conj klvs klv))))))

(def ^:const RandomIndexPack (from-hex "060e2b34020501010d01020101110100"))

(defn get-rip
  "Get the byte offsets of the partitions (usually header, body and footer)
   from the Random Index Pack (RIP; MXF book pg. 74-5) at the end of the file. Return nil if not present"
  ; XXX: obsolete?
  [^RandomAccessFile f]
    (let [length (.length f)
          off-from-end (do (.seek f (- length 4)) (.readInt f))
          read-offsets (fn [offset num]
                         (.seek f offset)
                         (let [b (byte-array num)]
                           (.readFully f b)
                           (map (fn [twelve] {:body-sid (bytes-to-num (subvec twelve 0 4)),
                                              :offset   (bytes-to-num (subvec twelve 4))})
                             (let [[[k l v] consumed] (parse-klv b 0 :raw-payload true)]
                               (when-not (= RandomIndexPack k)
                                 (throw (Exception. "this is not an RIP")))
                               (map vec (partition 12 v)))
                           )))]
      (if (< 4 off-from-end 10000)         ; sanity check
        (read-offsets (- length off-from-end) off-from-end)
        nil)))    ; RIP not present

(defn parse-partition-pack
  "Given a vector of bytes for the KLV payload of a PartitionPack, return its fields
   as a map like {:MajorVersion 1, :MinorVersion 2, :KAGSize 512, ...}."
  [v]
  (let [ec-count (bytes-to-num (subvec v 80 84)),
        ec-size  (bytes-to-num (subvec v 84 88))]
    (if (or (not= ec-size 16)
            (not= (* ec-count 16) (- (count v) 88)))
      (throw+ {:invalid-part-pack true,
               :ec-size ec-size,
               :ec-count ec-count,
               :payload-count (count v)})
      {:MajorVersion        (bytes-to-num (subvec v  0  2)),
       :MinorVersion        (bytes-to-num (subvec v  2  4)),
       :KAGSize             (bytes-to-num (subvec v  4  8)),
       :ThisPartition       (bytes-to-num (subvec v  8 16)),
       :PreviousPartition   (bytes-to-num (subvec v 16 24)),
       :FooterPartition     (bytes-to-num (subvec v 24 32)),
       :HeaderByteCount     (bytes-to-num (subvec v 32 40)),
       :IndexByteCount      (bytes-to-num (subvec v 40 48)),
       :IndexSID            (bytes-to-num (subvec v 48 52)),
       :BodyOffset          (bytes-to-num (subvec v 52 60)),
       :BodySID             (bytes-to-num (subvec v 60 64)),
       :OperationalPattern  (subvec v 64 80),
       :EssenceContainers   (parse-array  (subvec v 80) :ul)
      })))

(defn get-partition-packs
  "Gets all partition pack information by scanning the file for PartitionPack keys"
  [f & scanned]
  (vec
    (filter #(not (nil? %))
      (for [m (if (vector? scanned) scanned (quick-scan f))]
        (when (map? m)
          (let [ul (let [b (byte-array 16)]
                     (.seek f (:offset m))
                     (.readFully f b)
                     (vec b))]
            (when (partition-pack? ul)
              (let [payload (byte-array (:data-size m))]
                (.seek f (:data-start m))
                (.readFully f payload)
                (parse-partition-pack (vec payload))))))))))

(defn get-header
  "Get the file's header as a byte array"
  [^RandomAccessFile f & scanned]
  (let [scanned (if (vector? scanned) scanned (quick-scan f)),
        header-part-pack (first (get-partition-packs f scanned)),
        header-byte-count (:HeaderByteCount header-part-pack),
        primer-pack-offset (:offset (nth scanned 2)),  ; assuming PrimerPack is 3rd KLV FIXME not a good assumption! NLTek has PrimerPack as 2nd KLV
        kag (:KAGSize header-part-pack),  ; usually 0x100
;       header-size (let [unrounded (+ primer-pack-offset header-byte-count)]
;                     (if (pos? (bit-and unrounded (dec kag)))
;                       (+ (bit-and unrounded (bit-not (dec kag))) kag)
;                       unrounded)),
        header-size (:offset (first (filter #(<= (+ header-byte-count
                                                    primer-pack-offset)
                                                 (:offset %))
                                            scanned))),         ; ensure KLV boundary
        b (byte-array header-size)]
    (doto f
      (.seek 0)
      (.readFully b))
    b))

(defn hexify
  "Utility function to print klvs in a friendlier way"
  [coll]
  (w/prewalk
    (fn [x]
      (cond
        (byte-array? x) (to-hex (vec x))
        (#{:array, :batch} (get (meta x) :type)) x      ; don't print like "9abcdef0"
        (and (vector? x) (seq x) (every? number? x)) (to-hex x)
        :else x))
    coll))

(defn dump
  "Dump a textual representation of an MXF file to a .dump file.
   If :body true, then the entire body (not just the header) is written -- not
   recommended for large files, because the entire file will be read into memory."
  [filename & {:keys [body]}]
  (with-open [f (RandomAccessFile. filename "r")]
    (let [b (if body
              (let [b (byte-array (.length f))]
                (.readFully f b)
                b)
              (get-header f))
          [klvs consumed] (parse-klvs b 0)]
      (with-open [w (java.io.FileWriter. (str filename ".dump"))]
        (binding [*out* w]
          (doseq [klv klvs]
            (let [o (-> klv meta :offset)]
              (printf "; offset: 0x%08x (%d)\n" o o))
            (pprint (hexify klv))))))))

(defn -main
  "Display information about the MXF file"
  [& args]
  (if-not (= (count args) 1)
    (println "Expected MXF file as only argument")
    (let [filename (first args)]
      (with-open [f (RandomAccessFile. filename "r")]
        ;(prn (get-part-offsets f))
        ;;; TODO: display info
      ))))
