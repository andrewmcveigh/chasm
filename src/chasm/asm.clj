(ns chasm.asm
  (:require [clojure.java.io :as io]))

(def registers
  "https://www3.nd.edu/~dthain/courses/cse40243/fall2015/intel-intro.html
  X86-64 has sixteen (almost) general purpose 64-bit integer registers"
  [:%rax :%rbx :%rcx :%rdx :%rsi :%rdi :%rbp :%rsp :%r8 :%r9 :%r10 :%r11 :%r12 :%r13 :%r14 :%r15])

(def opcode-table
  ;; mne opc op  des
  [])

;; which opcodes/mnemonics do we support?

;; [pushq :%rbp]

(def e-ident
  [:ei-magic      (mapv byte "ELF")   ; magic number
   :ei-class      [2]                   ; 64 bit
   :ei-data       [1]                   ; little endian
   :ei-version    [1]                   ; original elf
   :ei-osabi      [1]                   ; system-v
   :ei-abiversion [0]                   ; undefined
   :ei-pad        [0 0 0 0 0 0 0]])     ; padding

(def e-type [2 0])          ; executable

(def e-machine [0x3E 0])                ; x86-64
(def e-version [1 0 0 0])               ; original
(def e-entry   [0xE8 0 0x40 0 0 0 0 0]) ; arbitrary? entry point location
(def e-phoff   [0x40 0 0 0 0 0 0 0])    ; program header offset (set per arch)
(def e-shoff   [0x60 0x02 0 0 0 0 0 0]) ; section header offset (arbitrary)
(def e-flags   [0 0 0 0])               ; EF_machine_flag CPU flags
(def e-ehsize  [0x40 0])                ; size of this header (64 bytes)

(def e-phentsize [0x38 0]) ; program header table entry size
(def e-phnum     [0x03 0]) ; number of entries in ^^
(def e-shentsize [0x40 0]) ; section header table entry size
(def e-shnum     [0x06 0]) ; number of entries in ^^
(def e-shstrndx  [0x03 0]) ; index of the section header table entry that contains the section names

(def file-header
  (->> (concat (->> e-ident (partition 2) (mapcat second))
               e-type
               e-machine
               e-version
               e-entry
               e-phoff
               e-shoff
               e-flags
               e-ehsize
               e-phentsize
               e-phnum
               e-shentsize
               e-shnum
               e-shstrndx
               )
       (byte-array)))

(def ph-entry-1 ; size 56
  (let [p-type   [0x01 0    0    0]
        p-flags  [0x05 0    0    0]
        p-offset [0    0    0    0 0 0 0 0]
        p-vaddr  [0    0    0x40 0 0 0 0 0]
        p-paddr  [0    0    0x40 0 0 0 0 0]
        p-filesz [0x0a 0x01 0    0 0 0 0 0]
        p-memsz  [0x0a 0x01 0    0 0 0 0 0]
        p-align  [0    0    0x20 0 0 0 0 0]]
    (concat p-type p-flags p-offset p-vaddr p-paddr p-filesz p-memsz p-align)))

(def ph-entry-2
  (let [p-type   [0x01 0 0       0]
        p-flags  [0x06 0 0       0]
        p-offset [0x0a 0x01 0    0 0 0 0 0]
        p-vaddr  [0x0a 0x01 0x60 0 0 0 0 0]
        p-paddr  [0x0a 0x01 0x60 0 0 0 0 0]
        p-filesz [0x0e 0    0    0 0 0 0 0]
        p-memsz  [0x0e 0    0    0 0 0 0 0]
        p-align  [0    0    0x20 0 0 0 0 0]]
    (concat p-type p-flags p-offset p-vaddr p-paddr p-filesz p-memsz p-align)))

(def program-header
  (byte-array
   (concat ph-entry-1 ph-entry-2)))

(defn write []
  (with-open [s (java.io.FileOutputStream. (io/file "target/elf"))]
    (.write s file-header)
    (.write s program-header)
    ))

(write)
