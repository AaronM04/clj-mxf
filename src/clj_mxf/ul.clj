(ns clj-mxf.ul
  (:use clojure.core)
  (:use [clj-mxf.slingshot-workaround :only [try-assoc+]])
  (:use [slingshot.slingshot :only [throw+ try+]])
; (:use [clj-mxf.bits :only [byte-to-uint8]])   ; actually not used
  (:gen-class))

(def ^:const UL_PREFIX [0x06, 0x0e, 0x2b, 0x34])
(def ^:const UMID_PREFIX [0x06, 0x0a, 0x2b, 0x34])

(defmacro from-hex
  "Hexadecimal string => vector of numbers. Chars outside [0-9a-fA-F]
   act as wildcards

   Example: (from-hex \"060e2b34025301vv0d01010101xxyy00\") =>
       [0x06, 0x0e, 0x2b, 0x34, 2, 0x53, 1, :vv, 0x0d, 1, 1, 1, 2, 8, 0, 0]"
  [s]
  (into []
    (for [[hi lo] (partition 2 s)]
      (cond
        (every?
          #(or (<= (int \0) % (int \9))
               (<= (int \a) % (int \f))
               (<= (int \A) % (int \F)))
          (map int [hi lo])) 
                (read-string (str "0x" hi lo))  ; use the reader
        :else (keyword (str hi lo))))))

(defn to-hex
  "vector of bytes => Hexadecimal string. Optional argument is a map
   of values for the keys"
  ([ul]
    (to-hex ul {}))
  ([ul values]
    (apply str (map #(format
                       "%02x"
                       (if (number? %)
                         %
                         (when-let [val (values %)]
                           val
                           0)))
                 ul))))

(comment   ;XXX
(defn find
  "Returns the next match of the byte array to the ul-bytes search pattern,
   or nil if none were found. The result is a map like {:offset 123, :vv 1},
   where :offset is the offset within the array where the result was found,
   and :vv is a key within ul-bytes (see documentation for from-hex macro)"
  [ul-bytes arr]
  (let [initial (subvec
                  ul-bytes
                  0 (let [idx (.indexOf
                                ul-bytes
                                (first (filter keyword? ul-bytes)))]
                      (if (pos? idx)
                        idx
                        (count ul-bytes))))
        pieces (partition (count initial) 1 arr)]
    (loop [i 0
           pieces pieces]
    ))));XXX

    
(declare known-uls known-tags)

(defn to-ulkey
  "Convert a vector of bytes to an UL keyword, or return nil if not found"
  [v]
  (loop [all known-uls]
    (let [[ulkey val] (first all)]
      (cond
        (some neg? v) (throw+ {:to-ulkey-error true,
                               :unexpected-negative true,
                               :vector v,
                               :msg "Should have called bytes-to-uint8"})
        (= v val) ulkey
        (empty? all) nil   ; not found
        :else (recur (rest all))))))

(defn lookup-tag
  "Given a tag number (uint16 like 0x3c0a), return the tuple
   (like [:InstanceUID, :id]) or nil if not found"
  [tagnum]
  (loop [all known-tags]
    (let [[tagkey val typ] (first all)]
      (if (and (not (nil? val)) (== tagnum val))
        [tagkey typ]
        (if (empty? all)
          nil   ; not found
          (recur (rest all)))))))

(defn partition-pack?
  "Returns true if the UL is a partition pack key. See mxf_labels_and_keys.h
   line 729."
  [ul]
  (and (= (subvec ul 0  7) [0x06, 0x0e, 0x2b, 0x34, 0x02, 0x05, 0x01]) ; regver byte is in between!
       (= (subvec ul 8 13) [0x0d, 0x01, 0x02, 0x01, 0x01])
       (contains? #{2 3 4} (nth ul 13))))    ; must be Header, Body, or Footer

; {:kind Header, :status OpenIncomplete}

(def known-uls
  "Vector of all known ULs. Entries are like [:ulkey [6, 0xe, ..]]"
  [
    [:OpenIncompleteHeader,    (from-hex "060e2b34020501010d01020101020100")],
    [:ClosedIncompleteHeader,  (from-hex "060e2b34020501010d01020101020200")],
    [:OpenCompleteHeader,      (from-hex "060e2b34020501010d01020101020300")],
    [:ClosedCompleteHeader,    (from-hex "060e2b34020501010d01020101020400")],

    [:OpenIncompleteBody,      (from-hex "060e2b34020501010d01020101030100")],
    [:ClosedIncompleteBody,    (from-hex "060e2b34020501010d01020101030200")],
    [:OpenCompleteBody,        (from-hex "060e2b34020501010d01020101030300")],
    [:ClosedCompleteBody,      (from-hex "060e2b34020501010d01020101030400")],

    [:OpenIncompleteFooter,    (from-hex "060e2b34020501010d01020101040100")],
    [:ClosedIncompleteFooter,  (from-hex "060e2b34020501010d01020101040200")],
    [:OpenCompleteFooter,      (from-hex "060e2b34020501010d01020101040300")],
    [:ClosedCompleteFooter,    (from-hex "060e2b34020501010d01020101040400")],

    [:PrimerPack,            (from-hex "060e2b34020501010d01020101050100")], ; [:array :ppentry]
    [:Sequence,              (from-hex "060e2b34025301010d01010101010f00")],
    [:SourceClip,            (from-hex "060e2b34025301010d01010101011100")],
    [:TimecodeComponent,     (from-hex "060e2b34025301010d01010101011400")],
    [:ContentStorage,        (from-hex "060e2b34025301010d01010101011800")],
    [:EssenceContainerData,  (from-hex "060e2b34025301010d01010101012300")],
    [:CDCIEssenceDescriptor, (from-hex "060e2b34025301010d01010101012800")],
    [:Preface,               (from-hex "060e2b34025301010d01010101012f00")],
    [:Identification,        (from-hex "060e2b34025301010d01010101013000")],
    [:NetworkLocator,        (from-hex "060e2b34025301010d01010101013200")],
    [:MaterialPackage,       (from-hex "060e2b34025301010d01010101013600")],
    [:SourcePackage,         (from-hex "060e2b34025301010d01010101013700")],
    [:Track,                 (from-hex "060e2b34025301010d01010101013b00")],
    [:MPEGVideoDescriptor,   (from-hex "060e2b34025301010d01010101015100")], ; CDCIEssenceDescriptor
    [:KLVFill                (from-hex "060e2b34010101010301021001000000")]
    [:UnknownAtEnd1          (from-hex "060e2b34025301010d01020101100100")] ;XXX

    [:AvidEssenceContainer   (from-hex "060e2b34010101ff4b464141000d4d4f")], ; used by NLTek
    ; NLTek
    [:NLTekPicEssenceCoding  (from-hex "060e2b34040101030401020201200204")],
    [:NLTekEssenceClpWrapped (from-hex "060e2b34010201010d01030115010701")],

    ; XDCAM
    [:UnkXDCAMFrameWrapped,  (from-hex "060e2b34025301010d01010101015100")], ; DV
    [:UnkXDCAMFrameWrappedV, (from-hex "060e2b34010201010d01030115010700")], ; MPEG4
    [:UnkXDCAMFrameWrappedA1,(from-hex "060e2b34010201010d01030116040a00")], ; with MPEG4
    [:UnkXDCAMFrameWrappedA2,(from-hex "060e2b34010201010d01030116040a01")], ; with MPEG4
    [:UnkXDCAMFrameWrappedA3,(from-hex "060e2b34010201010d01030116040a02")], ; with MPEG4
    [:UnkXDCAMFrameWrappedA4,(from-hex "060e2b34010201010d01030116040a03")], ; with MPEG4
    [:UnkXDCAM_XML,          (from-hex "060e2b34010101050301022001000000")], ; XXX if renaming this, update parse-klv cond at end
    [:UnkXDCAMArray8,        (from-hex "060e2b34025301010d01030114020100")], ; has array at offset 12

    ; Avid
    [:Avid01011bTypeDef,        (from-hex "060e2b34025301010d01010101011b00")],
    [:Avid01011cAudioInfo,      (from-hex "060e2b34025301010d01010101011c00")],
    [:Avid010120ContainerDef,   (from-hex "060e2b34025301010d01010101012000")],
    [:Avid010122,               (from-hex "060e2b34025301010d01010101012200")],
    [:Avid01012eBlankStr,       (from-hex "060e2b34025301010d01010101012e00")],
    [:Avid01013f,               (from-hex "060e2b34025301010d01010101013f00")],
    [:Avid01014cImportMetadata, (from-hex "060e2b34025301010d01010101014c00")],
    [:Avid020100ClassDef,       (from-hex "060e2b34025301010d01010102010000")],
    [:Avid020200PropertyDef,    (from-hex "060e2b34025301010d01010102020000")],
    [:Avid020400TypeDefInt,     (from-hex "060e2b34025301010d01010102040000")],
    [:Avid020500StrongObjRef,   (from-hex "060e2b34025301010d01010102050000")],
    [:Avid020600WeakObjRef,     (from-hex "060e2b34025301010d01010102060000")],
    [:Avid020700TypeDefEnum,    (from-hex "060e2b34025301010d01010102070000")], ; AARON
    [:Avid020800TypeDefFArr,    (from-hex "060e2b34025301010d01010102080000")],
    [:Avid020900TypeDefVArr,    (from-hex "060e2b34025301010d01010102090000")],
    [:Avid020a00TypeDefSet,     (from-hex "060e2b34025301010d010101020a0000")],
    [:Avid020b00TypeDefString,  (from-hex "060e2b34025301010d010101020b0000")],
    [:Avid020c00TypeDefStream,  (from-hex "060e2b34025301010d010101020c0000")],
    [:Avid020d00TypeDefRecord,  (from-hex "060e2b34025301010d010101020d0000")],
    [:Avid020e00TypeDefRename,  (from-hex "060e2b34025301010d010101020e0000")],
    [:Avid022000TypeDefExtEnum, (from-hex "060e2b34025301010d01010102200000")], ; TypeDefinitionExtendibleEnumeration
    [:Avid022100TypeDefIndirect,(from-hex "060e2b34025301010d01010102210000")],
    [:Avid022200TypeDefOpaque,  (from-hex "060e2b34025301010d01010102220000")],
    [:Avid022400MetaDef,        (from-hex "060e2b34025301010d01010102240000")],
    [:Avid022500MetaDict,       (from-hex "060e2b34025301010d01010102250000")], ; InterchangeObj
  ])

; types are :id, :date, :utf16str, [:array <type-or-size>], :unk, :ref, :ul, :umid, :rational, :uint8, :uint16, :uint32, :boolean, :int8, :int16, :int32, :uint64, [:batch <type-or-size>], :rgbalayout, :ppentry
(def known-tags
  "Vector of all known local set tags. Entries are like [:tagkey, 0x3c0a, :type]"
  [
    ; standard MXF. See MXF_ITEM_DEFINITION macro in mxf_baseline_data_model.h
;;    [:BPictureCount,    (from-hex "0000"), :uint16],     ; MPEGVideoDescriptor
;;    [:BitRate,          (from-hex "0000"), :uint32],     ; MPEGVideoDescriptor
;;    [:ClosedGOP,        (from-hex "0000"), :boolean],     ; MPEGVideoDescriptor
;;    [:CodedContentType, (from-hex "0000"), :uint8],     ; MPEGVideoDescriptor
;;    [:ConstantBFrames,  (from-hex "0000"), :boolean],     ; MPEGVideoDescriptor
;;    [:IdenticalGOP,     (from-hex "0000"), :boolean],     ; MPEGVideoDescriptor
;;    [:LowDelay,         (from-hex "0000"), :boolean],     ; MPEGVideoDescriptor
;;    [:MaxGOP,           (from-hex "0000"), :uint16],     ; MPEGVideoDescriptor
;;    [:ProfileAndLevel,  (from-hex "0000"), :uint8],     ; MPEGVideoDescriptor
;;    [:SingleSequence,   (from-hex "0000"), :boolean],     ; MPEGVideoDescriptor

    [:GenerationUID,    0x0102, :ref],     ; InterchangeObject
    [:DataDefinition,   0x0201, :ul],     ; StructuralComponent
    [:Duration,         0x0202, :uint64],     ; StructuralComponent
    [:EventStartPosition, 0x0601, :uint64],     ; DMSegment
    [:EventComment,     0x0602, :utf16str],     ; DMSegment
    [:StructuralComponents, 0x1001, [:array :ref]],     ; Sequence
    [:SourcePackageID,  0x1101, :umid],     ; SourceClip
    [:SourceTrackID,    0x1102, :uint32],     ; SourceClip
    [:StartPosition,    0x1201, :uint64],     ; SourceClip
    [:StartTimecode,    0x1501, :uint64],     ; TimecodeComponent
    [:RoundedTimecodeBase, 0x1502, :uint16],     ; TimecodeComponent
    [:DropFrame,        0x1503, :boolean],     ; TimecodeComponent
    [:Packages,         0x1901, [:batch :ref]],     ; ContentStorage
    [:EssenceContainerData, 0x1902, [:batch :ref]],     ; ContentStorage
    [:LinkedPackageUID, 0x2701, :umid],     ; EssenceContainerData
    [:Locators,         0x2f01, [:array :ref]],     ; GenericDescriptor
    [:SampleRate,       0x3001, :rational],     ; FileDescriptor
    [:ContainerDuration, 0x3002, :uint64],     ; FileDescriptor
    [:EssenceContainer, 0x3004, :ul],     ; FileDescriptor
    [:Codec,            0x3005, :ul],     ; FileDescriptor
    [:LinkedTrackID,    0x3006, :uint32],     ; FileDescriptor
    [:PictureEssenceCoding, 0x3201, :ul],     ; GenericPictureEssenceDescriptor
    [:StoredHeight,     0x3202, :uint32],     ; GenericPictureEssenceDescriptor
    [:StoredWidth,      0x3203, :uint32],     ; GenericPictureEssenceDescriptor
    [:SampledHeight,    0x3204, :uint32],     ; GenericPictureEssenceDescriptor
    [:SampledWidth,     0x3205, :uint32],     ; GenericPictureEssenceDescriptor
    [:SampledXOffset,   0x3206, :int32],     ; GenericPictureEssenceDescriptor
    [:SampledYOffset,   0x3207, :int32],     ; GenericPictureEssenceDescriptor
    [:DisplayHeight,    0x3208, :uint32],     ; GenericPictureEssenceDescriptor
    [:DisplayWidth,     0x3209, :uint32],     ; GenericPictureEssenceDescriptor
    [:DisplayXOffset,   0x320a, :int32],     ; GenericPictureEssenceDescriptor
    [:DisplayYOffset,   0x320b, :int32],     ; GenericPictureEssenceDescriptor
    [:FrameLayout,      0x320c, :uint8],     ; GenericPictureEssenceDescriptor
    [:VideoLineMap,     0x320d, [:array :int32]],     ; GenericPictureEssenceDescriptor
    [:AspectRatio,      0x320e, :rational],     ; GenericPictureEssenceDescriptor
    [:AlphaTransparency, 0x320f, :uint8],     ; GenericPictureEssenceDescriptor
    [:CaptureGamma,     0x3210, :ul],     ; GenericPictureEssenceDescriptor
    [:ImageAlignmentOffset, 0x3211, :uint32],     ; GenericPictureEssenceDescriptor
    [:FieldDominance,   0x3212, :uint8],     ; GenericPictureEssenceDescriptor
    [:ImageStartOffset, 0x3213, :uint32],     ; GenericPictureEssenceDescriptor
    [:ImageEndOffset,   0x3214, :uint32],     ; GenericPictureEssenceDescriptor
    [:SignalStandard,   0x3215, :uint8],     ; GenericPictureEssenceDescriptor
    [:StoredF2Offset,   0x3216, :int32],     ; GenericPictureEssenceDescriptor
    [:DisplayF2Offset,  0x3217, :int32],     ; GenericPictureEssenceDescriptor
    [:ActiveFormatDescriptor, 0x3218, :uint8],     ; GenericPictureEssenceDescriptor
    [:ColorPrimaries,   0x3219, :ul],     ; GenericPictureEssenceDescriptor
    [:CodingEquations,  0x321A, :ul],     ; GenericPictureEssenceDescriptor
    [:ComponentDepth,   0x3301, :uint32],     ; CDCIEssenceDescriptor
    [:HorizontalSubsampling, 0x3302, :uint32],     ; CDCIEssenceDescriptor
    [:ColorSiting,      0x3303, :uint8],     ; CDCIEssenceDescriptor
    [:BlackRefLevel,    0x3304, :uint32],     ; CDCIEssenceDescriptor
    [:WhiteReflevel,    0x3305, :uint32],     ; CDCIEssenceDescriptor
    [:ColorRange,       0x3306, :uint32],     ; CDCIEssenceDescriptor
    [:PaddingBits,      0x3307, :int16],     ; CDCIEssenceDescriptor
    [:VerticalSubsampling, 0x3308, :uint32],     ; CDCIEssenceDescriptor
    [:AlphaSampleDepth, 0x3309, :uint32],     ; CDCIEssenceDescriptor
    [:ReversedByteOrder, 0x330b, :boolean],     ; CDCIEssenceDescriptor
    [:PixelLayout,      0x3401, :rgbalayout],     ; RGBAEssenceDescriptor
    [:Palette,          0x3403, [:array :uint8]],     ; RGBAEssenceDescriptor
    [:PaletteLayout,    0x3404, :rgbalayout],     ; RGBAEssenceDescriptor
    [:ScanningDirection, 0x3405, :uint8],     ; RGBAEssenceDescriptor
    [:ComponentMaxRef,  0x3406, :uint32],     ; RGBAEssenceDescriptor
    [:ComponentMinRef,  0x3407, :uint32],     ; RGBAEssenceDescriptor
    [:AlphaMaxRef,      0x3408, :uint32],     ; RGBAEssenceDescriptor
    [:AlphaMinRef,      0x3409, :uint32],     ; RGBAEssenceDescriptor
    [:LastModifiedDate, 0x3b02, :date],     ; Preface
    [:ContentStorage,   0x3b03, :ref],     ; Preface
    [:Version,          0x3b05, :uint16],     ; Preface
    [:Identifications,  0x3b06, [:array :ref]],     ; Preface
    [:ObjectModelVersion, 0x3b07, :uint32],     ; Preface
    [:PrimaryPackage,   0x3b08, :ref],     ; Preface
    [:OperationalPattern, 0x3b09, :ul],     ; Preface
    [:EssenceContainers, 0x3b0a, [:batch :ul]],     ; Preface
    [:DMSchemes,        0x3b0b, [:batch :ul]],     ; Preface
    [:CompanyName,      0x3c01, :utf16str],     ; Identification
    [:ProductName,      0x3c02, :utf16str],     ; Identification
    [:ProductVersion,   0x3c03, :version],     ; Identification
    [:VersionString,    0x3c04, :utf16str],     ; Identification
    [:ProductUID,       0x3c05, :ref],     ; Identification
    [:ModificationDate, 0x3c06, :date],     ; Identification
    [:ToolkitVersion,   0x3c07, :version],     ; Identification
    [:Platform,         0x3c08, :utf16str],     ; Identification
    [:ThisGenerationUID, 0x3c09, :ref],     ; Identification
    [:InstanceUID,      0x3c0a, :ref],     ; InterchangeObject
    [:QuantizationBits, 0x3d01, :uint32],     ; GenericSoundEssenceDescriptor
    [:Locked,           0x3d02, :boolean],     ; GenericSoundEssenceDescriptor
    [:AudioSamplingRate, 0x3d03, :rational],     ; GenericSoundEssenceDescriptor
    [:AudioRefLevel,    0x3d04, :int8],     ; GenericSoundEssenceDescriptor
    [:ElectroSpatialFormulation, 0x3d05, :uint8],     ; GenericSoundEssenceDescriptor
    [:SoundEssenceCompression, 0x3d06, :ul],     ; GenericSoundEssenceDescriptor
    [:ChannelCount,     0x3d07, :uint32],     ; GenericSoundEssenceDescriptor
    [:AvgBps,           0x3d09, :uint32],     ; WaveAudioDescriptor
    [:BlockAlign,       0x3d0a, :uint16],     ; WaveAudioDescriptor
    [:SequenceOffset,   0x3d0b, :uint8],     ; WaveAudioDescriptor
    [:DialNorm,         0x3d0c, :int8],     ; GenericSoundEssenceDescriptor
    [:DataEssenceCoding, 0x3e01, :ul],     ; GenericDataEssenceDescriptor
    [:SubDescriptorUIDs, 0x3f01, [:array :ref]],     ; MultipleDescriptor
    [:IndexSID,         0x3f06, :uint32],     ; EssenceContainerData
    [:BodySID,          0x3f07, :uint32],     ; EssenceContainerData
    [:URLString,        0x4001, :utf16str],     ; NetworkLocator
    [:LocatorName,      0x4101, :utf16str],     ; TextLocator
    [:PackageUID,       0x4401, :umid],     ; GenericPackage
    [:Name,             0x4402, :utf16str],     ; GenericPackage
    [:Tracks,           0x4403, [:array :ref]],     ; GenericPackage
    [:PackageModifiedDate, 0x4404, :date],     ; GenericPackage
    [:PackageCreationDate, 0x4405, :date],     ; GenericPackage
    [:Descriptor,       0x4701, :ref],     ; SourcePackage
    [:TrackID,          0x4801, :uint32],     ; GenericTrack
    [:TrackName,        0x4802, :utf16str],     ; GenericTrack
    [:Sequence,         0x4803, :ref],     ; GenericTrack
    [:TrackNumber,      0x4804, :uint32],     ; GenericTrack
    [:EventEditRate,    0x4901, :rational],     ; EventTrack
    [:EventOrigin,      0x4902, :uint64],     ; EventTrack
    [:EditRate,         0x4b01, :rational],     ; Track
    [:Origin,           0x4b02, :uint64],     ; Track
    [:DMFramework,      0x6101, :ref],     ; DMSegment
    [:TrackIDs,         0x6102, [:batch :uint32]],     ; DMSegment
    [:DMSourceClipTrackIDs, 0x6103, [:batch :uint32]],     ; DMSourceClip
;;    [:GenerationUID,         0x0102, :ref],
;;    [:InstanceUID,           0x3c0a, :id],
;;    [:LastModifiedDate,      0x3b02, :date],
;;    [:PackageUID,            0x4401, :umid],       ; GenericPackage
;;    [:Name,                  0x4402, :utf16str],   ; GenericPackage
;;    [:PackageCreationDate,   0x4405, :date],       ; GenericPackage
;;    [:TrackName,             0x4802, :utf16str],
    ; Avid. see: mxf_avid_extensions_data_model.h
    [:AvidName,             0x0006, :utf16str], ; MetaDefinition
    [:AvidDesc,             0x0007, :utf16str], ; MetaDefinition
    [:AvidEnumValNames,     0x0015, :utf16str], ; AARON Avid020700TypeDefEnum
;    [:Avid0018,             0x0018, :utf16str],  ;have I seen this as a string before?
    [:AvidElementCount,     0x0018, :uint32], ; TypeDefinitionFixedArray
    [:AvidMemberNames,      0x001d, :utf16str], ; TypeDefinitionRecord
    [:Avid001f,             0x001f, :utf16str],
    [:Avid1b02,             0x1b02, :utf16str],
    [:Avid1b03,             0x1b03, :utf16str],
    [:Avid1e07,             0x1e07, :utf16str],
    [:Avid1e08,             0x1e08, :utf16str],
    [:Avid5001,             0x5001, :utf16str],
    [:Avidffe6,             0xffe6, :utf16str],
    [:AvidClassDefs,        0x0003, [:array :ref]], ; MetaDictionary
    [:AvidTypeDefs,         0x0004, [:array :ref]], ; MetaDictionary
    [:AvidProperties,       0x0009, [:array :ref]], ; ClassDefinition
    [:AvidRefTargetSet,     0x0013, [:array :ref]], ; TypeDefinitionWeakObjectReference
    [:AvidMemberTypes,      0x001c, [:array :ref]], ; TypeDefinitionRecord
    [:Avid0020,             0x0020, [:array :ref]],
    [:Avid2603,             0x2603, [:array :ref]],
    [:Avid2605,             0x2605, [:array :ref]],
    [:Avid2608,             0x2608, [:array :ref]],
    [:Avid260b,             0x260b, [:array :ref]],
    [:Avidffd5,             0xffd5, [:array :ref]],
    [:AvidImageSize,        0xfffc, :uint64], ; AvidFrameSampleSize*ContainerDuration XXX metadata says it's an int32 but the size is 8?!?
    [:AvidEnumValues,       0x0016, [:array :int64]], ; XXX AARON Avid020700TypeDefEnum
    [:Avid0001,             0x0001, :ref], ;XXX
    [:Avid0002PrefacePtr,   0x0002, :ref],
    [:AvidIdentification,   0x0005, :ul], ; MetaDefinition
    [:AvidParentClass,      0x0008, :ref], ; ClassDefinition
    [:AvidType              0x000b, :ref], ; PropertyDefinition
    [:AvidSReferencedType,  0x0011, :ref], ; TypeDefinitionStrongObjectReference
    [:AvidWReferencedType,  0x0012, :ref], ; TypeDefinitionWeakObjectReference
    [:AvidEnumElementType,  0x0014, :ref], ; AARON Avid020700TypeDefEnum
    [:AvidFArrElementType,  0x0017, :ref], ; TypeDefinitionFixedArray
    [:AvidVArrElementType,  0x0019, :ref], ; TypeDefinitionVariableArray
    [:AvidSetElementType,   0x001a, :ref], ; TypeDefinitionSet
    [:AvidStrElementType,   0x001b, :ref], ; TypeDefinitionString
    [:AvidRenamedType,      0x001e, :ref], ; TypeDefinitionRename
    [:Avid1b01,             0x1b01, :ref],
    [:AvidDataDef,          0x1e01, :ref], ; OperationDefinition
    [:Avid1e06,             0x1e06, :ref],
    [:Avidffd3,             0xffd3, :ref],
    [:AvidSize,             0x000f, :uint8], ; TypeDefinitionInteger
    [:Avid8000,             0x8000, :uint8],
    [:Avid8001,             0x8001, :uint32],
    [:AvidFrameSampleSize,  0xfffd, :uint32],
    [:AvidResolutionID,     0xfffe, :uint32],   ; or 0xfff4, depending on PropertyDef
    [:AvidSampleRate,       0xffff, :rational], ; AARON Preface
    [:AvidLocalID,          0x000d, :unk], ; uint16 PropertyDefinition
    [:AvidIsConcrete,       0x000a, :boolean], ; ClassDefinition
    [:AvidIsOptional,       0x000c, :boolean], ; PropertyDefinition
    [:AvidIsUniqueID,       0x000e, :boolean], ; PropertyDefinition
    [:AvidIsSigned,         0x0010, :boolean], ; TypeDefinitionInteger
    [:AvidIsTimeWarp,       0x1e02, :boolean], ; OperationDefinition
    [:Avid5003,             0x5003, :unk],   ; large (21-135 bytes)
    [:Avidffd4,             0xffd4, :unk],   ; 4 bytes
  ])


