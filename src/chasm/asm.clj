(ns chasm.asm
  (:require [clojure.java.io :as io]))

(def registers
  "https://www3.nd.edu/~dthain/courses/cse40243/fall2015/intel-intro.html
  X86-64 has sixteen (almost) general purpose 64-bit integer registers"
  [:%rax :%rbx :%rcx :%rdx :%rsi :%rdi :%rbp :%rsp :%r8 :%r9 :%r10 :%r11 :%r12 :%r13 :%r14 :%r15])

(def e-ident
  [:ei-magic      (mapv byte "ELF") ; magic number
   :ei-class      [2]                 ; 64 bit
   :ei-data       [1]                 ; little endian
   :ei-version    [1]                 ; original elf
   :ei-osabi      [1]                 ; system-v
   :ei-abiversion [0]                 ; undefined
   :ei-pad        [0 0 0 0 0 0 0]])   ; padding

(defn e-type-f [t]
  (case t
    :relocatable 1
    :executable  2
    :shared      3
    :core        4))

(def e-type      (e-type-f :executable))
(def e-machine   0x3e)     ; x86-64
(def e-version   1)        ; original
(def e-entry     0x4000e8) ; arbitrary? entry point location
(def e-phoff     64)       ; program header offset (64 bit arch = 64 bytes)
(def e-flags     0)        ; EF_machine_flag CPU flags
(def e-ehsize    64)       ; size of this header (64 bytes)

(defn bsplit
  "Split integer x into n byte parts"
  [x n]
  (if (<= n 1)
    (list x)
    (conj (bsplit (bit-shift-right x 8) (dec n))
          (bit-and x 0xff))))

(defn bunsplit
  "Unsplit seq of bytes into integer"
  [xs]
  (loop [[x & xs] (reverse xs) y 0]
    (if x
      (recur xs (+ (bit-shift-left y 8) x))
      y)))

(defn bunsplit-h
  [xs]
  (symbol (str "0x" (Integer/toHexString (bunsplit xs)))))

(defn e-shoff [program-offset program-size]
  (+ program-offset program-size))

(defn build-file-header
  [program-header program-size section-header section-names-idx]
  (let [ident       (->> e-ident (partition 2) (mapcat second))
        e-phentsize (count (first program-header)) ; a program header table entry size
        e-phnum     (count program-header) ; number of entries in ^^
        e-shentsize (count (first section-header)) ; a section header table entry size
        e-shnum     (count section-header) ; number of entries in ^^
        e-shstrndx  (bsplit section-names-idx 2)
        ;; ^^ index of the section header table entry that contains the section names
        e-shoff     (-> e-phoff ; section header offset, after program data
                        (+ (* e-phnum e-phentsize))
                        (e-shoff program-size))]
    (byte-array
     (concat ident
             (bsplit e-type      2)
             (bsplit e-machine   2)
             (bsplit e-version   4)
             (bsplit e-entry     8)
             (bsplit e-phoff     8)
             (bsplit e-shoff     8)
             (bsplit e-flags     4)
             (bsplit e-ehsize    2)
             (bsplit e-phentsize 2)
             (bsplit e-phnum     2)
             (bsplit e-shentsize 2)
             (bsplit e-shnum     2)
             e-shstrndx))))

(defn program-header-entry
  [& {:keys [type flags offset vaddr paddr filesz memsz align]}]
  (concat
   (bsplit type   4) (bsplit flags  4) (bsplit offset 8) (bsplit vaddr  8)
   (bsplit paddr  8) (bsplit filesz 8) (bsplit memsz  8) (bsplit align  8)))

(def program-header
  [(program-header-entry
    :type   0x01     :flags  0x05
    :vaddr  0x400000 :paddr  0x400000
    :offset 0x00     :align  0x200000
    :filesz 0x010a   :memsz  0x010a)
   (program-header-entry
    :type   0x01     :flags 0x06
    :vaddr  0x60010a :paddr 0x60010a
    :offset 0x10a    :align 0x200000
    :filesz 0x0e     :memsz 0x0e)
   (program-header-entry
    :type   0x65041580 :flags 0x2800
    :vaddr  0x00       :paddr 0x00
    :offset 0x00       :align 0x08
    :filesz 0x00       :memsz 0x00)])

(def segment-1 ; exectutable
  [0xba 0x0e  0x00 0x00 0x00
   ;; movl %edx len 14
   0xb9 0x0a  0x01 0x60 0x00
   ;; movl %ecx ptr to Hello, world!\n
   0xbb 0x01  0x00 0x00 0x00
   ;; movl %ebx 1 stdout
   0xb8 0x04  0x00 0x00 0x00
   ;; movl %eax 4 sys_write
   0xcd 0x80
   ;; int 0x80 call kernel and exit
   0xbb 0x00  0x00 0x00 0x00
   ;; movl %ebx 0
   0xb8 0x01  0x00 0x00 0x00
   ;; movl %eax 1
   0xcd 0x80])
;; int 0x80 call kernel

(def segment-2 ; Hello, world!\n
  [0x48 0x65  0x6c 0x6c  0x6f 0x2c  0x20 0x77
   0x6f 0x72  0x6c 0x64  0x21 0x0a])

(def string-table-4 ;  .symtab .strtab .shstrtab .text .data  
  [0x00 0x2e 0x73 0x79 0x6d 0x74 0x61 0x62
   0x00 0x2e 0x73 0x74 0x72 0x74 0x61 0x62
   0x00 0x2e 0x73 0x68 0x73 0x74 0x72 0x74
   0x61 0x62 0x00 0x2e 0x74 0x65 0x78 0x74
   0x00 0x2e 0x64 0x61 0x74 0x61 0x00 0x00])

(def symbol-table-5
  [0x00 0x00  0x00 0x00  0x00 0x00  0x00 0x00
   0x00 0x00  0x00 0x00  0x00 0x00  0x00 0x00
   0x00 0x00  0x00 0x00  0x00 0x00  0x00 0x00
   0x00 0x00  0x00 0x00  0x03 0x00  0x01 0x00
   0xe8 0x00  0x40 0x00  0x00 0x00  0x00 0x00
   0x00 0x00  0x00 0x00  0x00 0x00  0x00 0x00
   0x00 0x00  0x00 0x00  0x03 0x00  0x02 0x00
   0x0a 0x01  0x60 0x00  0x00 0x00  0x00 0x00
   0x00 0x00  0x00 0x00  0x00 0x00  0x00 0x00
   0x01 0x00  0x00 0x00  0x04 0x00  0xf1 0xff
   0x00 0x00  0x00 0x00  0x00 0x00  0x00 0x00
   0x00 0x00  0x00 0x00  0x00 0x00  0x00 0x00
   0x10 0x00  0x00 0x00  0x00 0x00  0xf1 0xff
   0x0e 0x00  0x00 0x00  0x00 0x00  0x00 0x00
   0x00 0x00  0x00 0x00  0x00 0x00  0x00 0x00
   0x14 0x00  0x00 0x00  0x00 0x00  0x02 0x00
   0x0a 0x01  0x60 0x00  0x00 0x00  0x00 0x00
   0x00 0x00  0x00 0x00  0x00 0x00  0x00 0x00
   0x1d 0x00  0x00 0x00  0x10 0x00  0x01 0x00
   0xe8 0x00  0x40 0x00  0x00 0x00  0x00 0x00
   0x00 0x00  0x00 0x00  0x00 0x00  0x00 0x00
   0x18 0x00  0x00 0x00  0x10 0x00  0x02 0x00
   0x18 0x01  0x60 0x00  0x00 0x00  0x00 0x00
   0x00 0x00  0x00 0x00  0x00 0x00  0x00 0x00
   0x24 0x00  0x00 0x00  0x10 0x00  0x02 0x00
   0x18 0x01  0x60 0x00  0x00 0x00  0x00 0x00
   0x00 0x00  0x00 0x00  0x00 0x00  0x00 0x00
   0x2b 0x00  0x00 0x00  0x10 0x00  0x02 0x00
   0x18 0x01  0x60 0x00  0x00 0x00  0x00 0x00
   0x00 0x00  0x00 0x00  0x00 0x00  0x00 0x00])

(def string-table-6 ;  target/hello.o len msg __bss_start _edata _end 
  [0x00 0x74  0x61 0x72  0x67 0x65  0x74 0x2f
   0x68 0x65  0x6c 0x6c  0x6f 0x2e  0x6f 0x00
   0x6c 0x65  0x6e 0x00  0x6d 0x73  0x67 0x00
   0x5f 0x5f  0x62 0x73  0x73 0x5f  0x73 0x74
   0x61 0x72  0x74 0x00  0x5f 0x65  0x64 0x61
   0x74 0x61  0x00 0x5f  0x65 0x6e  0x64 0x00])

(def sections
  [segment-1 segment-2 symbol-table-5 string-table-6 string-table-4])

(defmacro fed [label code desc]
  `(def ~label ~desc ~code))

(fed SHT_NULL          0x00       "Section header table entry unused")
(fed SHT_PROGBITS      0x01       "Program data")
(fed SHT_SYMTAB        0x02       "Symbol table")
(fed SHT_STRTAB        0x03       "String table")
(fed SHT_RELA          0x04       "Relocation entries with addends")
(fed SHT_HASH          0x05       "Symbol hash table")
(fed SHT_DYNAMIC       0x06       "Dynamic linking information")
(fed SHT_NOTE          0x07       "Notes")
(fed SHT_NOBITS        0x08       "Program space with no data (bss)")
(fed SHT_REL           0x09       "Relocation entries, no addends")
(fed SHT_SHLIB         0x0A       "Reserved")
(fed SHT_DYNSYM        0x0B       "Dynamic linker symbol table")
(fed SHT_INIT_ARRAY    0x0E       "Array of constructors")
(fed SHT_FINI_ARRAY    0x0F       "Array of destructors")
(fed SHT_PREINIT_ARRAY 0x10       "Array of pre-constructors")
(fed SHT_GROUP         0x11       "Section group")
(fed SHT_SYMTAB_SHNDX  0x12       "Extended section indeces")
(fed SHT_NUM           0x13       "Number of defined types.")
(fed SHT_LOOS          0x60000000 "Start OS-specific.")

(fed SHF_WRITE            0x01       "Writable")
(fed SHF_ALLOC            0x02       "Occupies memory during execution")
(fed SHF_EXECINSTR        0x04       "Executable")
(fed SHF_MERGE            0x10       "Might be merged")
(fed SHF_STRINGS          0x20       "Contains nul-terminated strings")
(fed SHF_INFO_LINK        0x40       "'sh_info' contains SHT index")
(fed SHF_LINK_ORDER       0x80       "Preserve order after combining")
(fed SHF_OS_NONCONFORMING 0x0100     "Non-standard OS specific handling required")
(fed SHF_GROUP            0x0200     "Section is member of a group")
(fed SHF_TLS              0x0400     "Section hold thread-local data")
(fed SHF_MASKOS           0x0ff00000 "OS-specific")
(fed SHF_MASKPROC         0xf0000000 "Processor-specific")
(fed SHF_ORDERED          0x04000000 "Special ordering requirement (Solaris)")
(fed SHF_EXCLUDE          0x08000000 "Section is excluded unless referenced or allocated (Solaris)")

(defn section-header-entry
  [& {:keys [name type flags addr offset size link info addralign entsize]}]
  (concat
   (bsplit name      4) (bsplit type      4) (bsplit flags 8) (bsplit addr 8)
   (bsplit offset    8) (bsplit size      8) (bsplit link  4) (bsplit info 4)
   (bsplit addralign 8) (bsplit entsize   8)))

(def section-header
  [(section-header-entry ; null
    :name    0x00        :type      0x00       :flags 0x00
    :addr    0x00        :offset    0x00       :size  0x00
    :link    0x00        :info      0x00
    :entsize 0x00        :addralign 0x00)
   (section-header-entry ; segment-1
    :name      0x1b      :type    SHT_PROGBITS :flags (+ SHF_ALLOC SHF_EXECINSTR)
    :addr      0x4000e8  :offset  0xe8         :size  0x22
    :link      0x00      :info    0x00
    :addralign 0x01      :entsize 0x00)
   (section-header-entry ; segment-2 Hello, world!\n
    :name      0x21      :type    SHT_PROGBITS :flags (+ SHF_WRITE SHF_ALLOC)
    :addr      0x60010a  :offset  0x010a       :size  0x0e
    :link      0x00      :info    0x00
    :addralign 0x01      :entsize 0x00)
   (section-header-entry
    :name      0x11      :type    SHT_STRTAB   :flags 0x00
    :addr      0x00      :offset  0x0238       :size  0x27
    :link      0x00      :info    0x00
    :addralign 0x01      :entsize 0x00)
   (section-header-entry
    :name      0x01      :type    SHT_SYMTAB   :flags 0x00
    :addr      0x00      :offset  0x118        :size  0xf0
    :link      0x05      :info    0x06
    :addralign 0x08      :entsize 0x18)
   (section-header-entry
    :name      0x09      :type    SHT_STRTAB   :flags 0x00
    :addr      0x00      :offset  0x0208       :size  0x30
    :link      0x00      :info    0x00
    :addralign 0x01      :entsize 0x00)])

(defn write []
  (let [program  (apply concat sections)
        file-hdr (build-file-header program-header
                                    (count program)
                                    section-header
                                    3)
        f (io/file "target/elf")]
    (with-open [s (java.io.FileOutputStream. f)]
      (.write s file-hdr)
      (.write s (byte-array (apply concat program-header)))
      (.write s (byte-array program))
      (.write s (byte-array (apply concat section-header))))
    (.setExecutable f true true)))

(write)
