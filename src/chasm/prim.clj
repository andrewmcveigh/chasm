(ns chasm.prim
  (:refer-clojure :exclude [print read])
  (:require
   [chasm.elf :as elf]
   [chasm.util :as u]))

;;; TODO: primitives are implemented by the compiler

(defn mk-lambda
  "A λ is a 1-arg `x` function with expression `e`. It captures outer lexical
  bindings in a closure."
  [x e])

;; do we need a basic cons? - yes at first at least
;; we need to be able to create data structures

(defprotocol Representable
  (to-bytes [x]))

(extend-protocol Representable
  Byte
  (to-bytes [x] (u/bsplit x 1))
  Character
  (to-bytes [x] (u/bsplit x 1))
  Short
  (to-bytes [x] (u/bsplit x 2))
  Integer
  (to-bytes [x] (u/bsplit x 4))
  Long
  (to-bytes [x] (u/bsplit x 8))
  String
  (to-bytes [x] (mapv byte x))
  clojure.lang.ISeq
  (to-bytes [x] (mapv byte x))
  )

(def null 0x600000)

(def mem (atom {:offset 0
                :bytes  []}))

(deftype Ptr [x])

(defn alloc! [x]
  (let [offset (:offset @mem)
        x'     (to-bytes x)
        length (count x')]
    (swap! mem update :offset + length)
    (swap! mem update :bytes into x')
    [(Ptr. {:val offset}) length]))

(defn +cons
  "A cons cell is two contiguous memory locations, the first containing a value,
  and the second containing a pointer to the next cons cell, or nil to
  terminate.
  `a` should be a value
  `b` should either be a cons cell, or nil"
  [a b]
  (let [ptr-a   (alloc! a)
        [ptr-b] (or b [null])]
    [ptr-a ptr-b]))

;; (+cons "Hello, " (+cons "World!\n" nil))

;;; TODO: what even is a program?
;;; it's a tree of applications

;;; A simple program
;;; read 2 numbers from stdin
;;; print the addition to std out

(defn add-8 [a b]
  [0xb6 (u/bsplit a 1) ;; movb %ah a
   0xb7 (u/bsplit b 1) ;; movb %al b
   ;;
   ]
  ;; got 2 numbers
  ;; put 1 in an 8 bit register
  ;; put other in an add register
  ;; add
  ;; put the result somewhere
  )

;; where do `a` & `b` come from?
;; where does `ret` go? - it goes to the caller - the calling function must know
;; where to get it

 ;; (defn mov-rax [x]
 ;;   [0x48 0xc7 0xc0 (u/bsplit x 4)]))

(def r64
  [:rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi
   :r8  :r9  :r10 :r11 :r12 :r13 :r14 :r15])

(def r32
  [:eax :ebx :ecx  :edx  :ebp  :esi  :edi  :esp
   :r8d :r9d :r10d :r11d :r12d :r13d :r14d :r15d])

(defn movq [r x]
  (let [reg (fn [r] (+ 0xc0 (.indexOf r64 r)))]
    [0x48 0xc7 (reg r) (if (instance? Ptr x)
                         (Ptr. (assoc (.-x x) :bsplit 4))
                         (u/bsplit x 4))]))

(defmulti movq (fn [r x] (type x)))

(defn movq-integer [r x]
  (let [reg (+ 0xc0 (.indexOf r64 r))]
    [0x48 0xc7 reg (u/bsplit x 4)]))

(defmethod movq Long [r x]
  (movq-integer r x))

(defmethod movq Integer [r x]
  (movq-integer r x))

(defmethod movq Ptr [r x]
  (let [reg (+ 0xc0 (.indexOf r64 r))]
    [0x48 0xc7 reg (Ptr. (assoc (.-x x) :bsplit 4))]))


;; put contents of x in r
(defmethod movq clojure.lang.Keyword [r x]
  (let [ir (.indexOf r64 r)
        ix (.indexOf r64 x)]
    (cond (and (< ir 8) (< ix 8))
          [0x48 0x89 (+ 0xc0 (* 8 ir) ix)]
          (and (< ir 8) (>= ix 8))
          [0x49 0x89 (+ 0xc0 (* 8 ir) (- ix 8))]
          (and (>= ir 8) (< ix 8))
          [0x4c 0x89 (+ 0xc0 (* 8 (- ir 8)) ix)]
          (and (>= ir 8) (>= ix 8))
          [0x4d 0x89 (+ 0xc0 (* 8 (- ir 8)) (- ix 8))])))

;; data Reg
;;   = RAX  -- Accumulator
;;   | RCX  -- Counter (Loop counters)
;;   | RDX  -- Data
;;   | RBX  -- Base / General Purpose
;;   | RSP  -- Current stack pointer
;;   | RBP  -- Previous Stack Frame Link
;;   | RSI  -- Source Index Pointer
;;   | RDI  -- Destination Index Pointer
;;   deriving (Eq, Show)

(defn movl [r x]
  (let [reg {:eax 0xb8
             :ebx 0xbb
             :ecx 0xb9
             :edx 0xba}]
    [(reg r) (u/bsplit x 4)]))

(defn exit [code]
  [(movq :rbx code)
   ;; movl %ebx code
   (movq :rax 1)
   ;; movl %eax 1
   0xcd  0x80])

(defn read [ptr len]
  [(movq :rax 0)
   ;; clear rax
   (movq :rdx len)
   (movq :rcx ptr)
   (movq :rbx 0) ;; stdin
   (movq :rax 3) ;; sys_read
   0xcd  0x80])

(defn print [ptr len]
  [(movq :rax :rdx)
   ;; movl %edx len 14
   (movq :rcx ptr)
   ;; movl %ecx ptr to Hello, world!\n
   (movq :rbx 1)
   ;; movl %ebx 1 stdout
   (movq :rax 4)
   ;; movl %eax 4 sys_write
   0xcd  0x80])

;; (defn print-rax-len [ptr]
;;   [0xba  (u/bsplit len 4)
;;    ;; movl %edx len 14
;;    0xb9  (u/bsplit ptr 4)
;;    ;; movl %ecx ptr to Hello, world!\n
;;    0xbb  [0x01 0x00 0x00 0x00]
;;    ;; movl %ebx 1 stdout
;;    0xb8  [0x04 0x00 0x00 0x00]
;;    ;; movl %eax 4 sys_write
;;    0xcd  [0x80]]
;;   )

(defn buffer [n]
  (repeat n 0))
;; ^^ to-bytes

(def program
  (let [[ptr len] (alloc! (buffer 32))]
    (flatten
     [(read ptr len)
      (print ptr nil)
      (exit 0)])))

(defn program-size [program]
  (->> program
       (map (fn [x]
              (if (instance? Ptr x)
                (let [{:keys [val bsplit]} (.-x x)]
                  (u/bsplit val bsplit))
                x)))
       (flatten)
       (count)))

(defn relocate-ptrs [program]
  (let [offset (+ 0x600000
                  elf/e-ehsize
                  (* 3 56)
                  (program-size program))]
    (->> program
         (map (fn [x]
                (if (instance? Ptr x)
                  (let [{:keys [val bsplit]} (.-x x)]
                    (u/bsplit (+ offset val) bsplit))
                  x)))
         (flatten))))

(elf/write "target/elf"
           (relocate-ptrs program)
           (:bytes @mem))

;; what is a call stack?
;; what is the stack?
;; what is the heap?

;; something gets allocated to the stack when:
;; * it's a primitive value or pointer passed to a function
;; * it's a return primitive value or pointer

;; what is a function?
;; - a function is:
;;   * some code - instructions in memory, with an address
;;   * bindings - some memory with an address
;;                with maybe pointers to other stuff
;;   the intuition is a function is a pointer with meta data
;; what is a pointer?
;; - a pointer is a 64 bit integer, to a memory location
;; how can it have meta data?
;; - it's a pointer to a data structure that contains:
;;   a. executable code - data
;;   b. other data

;; what is a function call?
;; - the same as a function call in an intepreter?

;; what is a primitive function?

;; is there a difference?

;; what is a primitive call?

;; what is a value?
;; - a value is some data in memory, it needs an address
;; - or it's a value on the stack? Just an actual value
