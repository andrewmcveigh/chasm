(ns chasm.elf
  (:require [clojure.java.io :as io]))

(defmacro fed [label code desc]
  `(def ~label ~desc ~code))

(def PT_NULL    0x00000000)
(def PT_LOAD    0x00000001)
(def PT_DYNAMIC 0x00000002)
(def PT_INTERP  0x00000003)
(def PT_NOTE    0x00000004)
(def PT_SHLIB   0x00000005)
(def PT_PHDR    0x00000006)
(def PT_LOOS    0x60000000)
(def PT_HIOS    0x6FFFFFFF)
(def PT_LOPROC  0x70000000)
(def PT_HIPROC  0x7FFFFFFF)

(fed PF_X        0x1        "Execute")
(fed PF_W        0x2        "Write")
(fed PF_R        0x4        "Read")
(fed PF_MASKOS   0x0ff00000 "Unspecified")
(fed PF_MASKPROC 0xf0000000 "Unspecified")

(fed SHT_NULL             0x00       "Section header table entry unused")
(fed SHT_PROGBITS         0x01       "Program data")
(fed SHT_SYMTAB           0x02       "Symbol table")
(fed SHT_STRTAB           0x03       "String table")
(fed SHT_RELA             0x04       "Relocation entries with addends")
(fed SHT_HASH             0x05       "Symbol hash table")
(fed SHT_DYNAMIC          0x06       "Dynamic linking information")
(fed SHT_NOTE             0x07       "Notes")
(fed SHT_NOBITS           0x08       "Program space with no data (bss)")
(fed SHT_REL              0x09       "Relocation entries, no addends")
(fed SHT_SHLIB            0x0A       "Reserved")
(fed SHT_DYNSYM           0x0B       "Dynamic linker symbol table")
(fed SHT_INIT_ARRAY       0x0E       "Array of constructors")
(fed SHT_FINI_ARRAY       0x0F       "Array of destructors")
(fed SHT_PREINIT_ARRAY    0x10       "Array of pre-constructors")
(fed SHT_GROUP            0x11       "Section group")
(fed SHT_SYMTAB_SHNDX     0x12       "Extended section indeces")
(fed SHT_NUM              0x13       "Number of defined types.")
(fed SHT_LOOS             0x60000000 "Start OS-specific.")

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

(def e-entry
  "The link editor (a.k.a static linker), ld(1), ensures that an executable's
  loadable code segment always starts at a certain virtual address. The value
  is architecture dependent. For x86, 0x08048000 (for 32-bit address spaces)
  and 0x400000 (for 64-bit address spaces) (+ 0xe8 offset)"
  0x4000e8)

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
  (let [s (Integer/toHexString (bunsplit xs))
        p (if (zero? (mod (count s) 2)) "" "0")]
    (symbol (str "0x" p s))))

(defn program-header-entry
  [& {:keys [type flags offset vaddr paddr filesz memsz align]}]
  (concat
   (bsplit type   4) (bsplit flags  4) (bsplit offset 8) (bsplit vaddr  8)
   (bsplit paddr  8) (bsplit filesz 8) (bsplit memsz  8) (bsplit align  8)))

(defn section-header-entry
  [& {:keys [name type flags addr offset size link info addralign entsize]}]
  (concat
   (bsplit name      4) (bsplit type      4) (bsplit flags 8) (bsplit addr 8)
   (bsplit offset    8) (bsplit size      8) (bsplit link  4) (bsplit info 4)
   (bsplit addralign 8) (bsplit entsize   8)))

(def program-instructions
  [0xba  0x0e 0x00 0x00 0x00 ;; 0x0000000e
   ;; movl %edx len 14
   0xb9  0x0a 0x01 0x60 0x00 ;; 0x0060010a <- so we have to know the ptr address
   ;; movl %ecx ptr to Hello, world!\n        but it's unknown until alloc'd
   0xbb  0x01 0x00 0x00 0x00
   ;; movl %ebx 1 stdout
   0xb8  0x04 0x00 0x00 0x00
   ;; movl %eax 4 sys_write
   0xcd  0x80
   ;; int 0x80 call kernel and exit
   0xbb  0x00 0x00 0x00 0x00
   ;; movl %ebx 0
   0xb8  0x01 0x00 0x00 0x00
   ;; movl %eax 1
   0xcd  0x80
   ])
;; int 0x80 call kernel

(def hello-world-bytes
  (map byte "Hello, World!\n"))

(defn program [instructions data]
  "An executable or shared object file's program header table is an array of
   structures, each describing a segment or other information the system needs
   to prepare the program for execution. An object file segment contains one or
   more sections."
  (let [file-header-size e-ehsize
        prog-hdr-offset  e-phoff
        prog-hdr-count   3 ; instructions + data + end
        prog-hdr-size    56
        instructs-count  (count instructions)
        filesz-1         (+ e-ehsize (* 3 56) instructs-count)
        filesz-2         (count data)
        program          (concat instructions data)]
    {:header [(program-header-entry
               :type   PT_LOAD    :flags (+ PF_X PF_R) ;;; this tells the start of program
               :vaddr  0x400000   :paddr 0x400000
               :offset 0x00       :align 0x200000
               :filesz filesz-1   :memsz filesz-1) ;;; 266, why?
              (program-header-entry ;;; this tells where the text is
               :type   PT_LOAD    :flags (+ PF_W PF_R)
               :vaddr  0x60010a   :paddr 0x60010a ; address where text starts
               :offset 0x010a     :align 0x200000 ; could/should this be decided
               :filesz filesz-2   :memsz filesz-2) ; by the complier - yes?
              (program-header-entry ;;; dunno what this is for
               :type   0x65041580 :flags 0x2800
               :vaddr  0x00       :paddr 0x00
               :offset 0x00       :align 0x08
               :filesz 0x00       :memsz 0x00)]
     :program {:bytes program :size (count program)}}))

(defn file-header-meta
  [program-header program-size section-header section-names-idx]
  (let [e-phentsize (count (first program-header))
        e-phnum     (count program-header)
        e-shnum     (count section-header)]
    {:e-ident     (->> e-ident (partition 2) (mapcat second))
     :e-phentsize e-phentsize ; a program header table entry size
     :e-phnum     e-phnum         ; number of entries in ^^
     :e-shentsize (count (first section-header)) ; a section header table entry size
     :e-shnum     e-shnum         ; number of entries in ^^
     :e-shstrndx  (bsplit section-names-idx 2)
     ;; ^^ index of the section header table entry that contains the section names
     :e-shoff     (if (zero? e-shnum)
                    0
                    (-> e-phoff ; section header offset, after program data
                        (+ (* e-phnum e-phentsize))
                        (+ program-size)))}))

(defn file-header
  [{:keys [e-ident e-phentsize e-phnum e-shentsize e-shnum e-shstrndx e-shoff]}]
  [e-ident
   (bsplit e-type  2) (bsplit e-machine   2) (bsplit e-version   4)
   (bsplit e-entry 8) (bsplit e-phoff     8) (bsplit e-shoff     8)
   (bsplit e-flags 4) (bsplit e-ehsize    2) (bsplit e-phentsize 2)
   (bsplit e-phnum 2) (bsplit e-shentsize 2) (bsplit e-shnum     2)
   e-shstrndx])

(defn write [filename & program-parts]
  (let [{:keys [header program]} (apply program program-parts)
        file-hdr-meta  (file-header-meta header (:size program) nil 0)
        file-hdr       (file-header file-hdr-meta)
        target-file    (io/file filename)]
    (with-open [s (java.io.FileOutputStream. target-file)]
      (.write s (byte-array (apply concat file-hdr)))
      (.write s (byte-array (apply concat header)))
      (.write s (byte-array (:bytes program))))
    (.setExecutable target-file true true)))

;; (write "target/elf" program-instructions hello-world-bytes)

(def buffer (repeat 1000 0))

(def program-instructions
  [0xba  0x0e 0x00 0x00 0x00 ;; 0x0000000e
   ;; movl %edx len 14
   0xb9  0x0a 0x01 0x60 0x00 ;; 0x0060010a <- so we have to know the ptr address
   ;; movl %ecx ptr to Hello, world!\n        but it's unknown until alloc'd
   0xbb  0x01 0x00 0x00 0x00
   ;; movl %ebx 1 stdout
   0xb8  0x04 0x00 0x00 0x00
   ;; movl %eax 4 sys_write
   0xcd  0x80
   ;; int 0x80 call kernel and exit
   0xbb  0x00 0x00 0x00 0x00
   ;; movl %ebx 0
   0xb8  0x01 0x00 0x00 0x00
   ;; movl %eax 1
   0xcd  0x80
   ])
;; int 0x80 call kernel

;; (def program-2
;;   [0xba  0x0f 0x00 0x00 0x00
;;    ;; mov  edx, 1             ; max length
;;    0xb9  0x0a 0x01 0x60 0x00
;;    ;; mov  ecx, buf           ; buffer
;;    0xbb  0x00 0x00 0x00 0x00
;;    ;; mov  ebx, 0             ; stdin
;;    0xb8  0x03 0x00 0x00 0x00
;;    ;; mov  eax, 3             ; sys_read
;;    0xcd  0x80
;;    ;; int  80h
;;    0xba  0x03 0x00 0x00 0x00
;;    ;; mov  edx, eax           ; length
;;    0xb9  0x0a 0x01 0x60 0x00
;;    ;; mov  ecx, buf           ; buffer
;;    0xbb  0x01 0x00 0x00 0x00
;;    ;; mov  ebx, 1             ; stdout
;;    0xb8  0x04 0x00 0x00 0x00
;;    ;; mov  eax, 4             ; sys_write
;;    0xcd  0x80
;;    ;; int  80h
;;    (exit 0)
;;    ])

;; (write "target/rp" (flatten program-2) buffer)
