(ns chasm.prim
  (:refer-clojure :exclude [empty extend get print read])
  (:require
   [chasm.elf :as elf]
   [chasm.util :as u]
   [clojure.spec.alpha :as s]
   [clojure.core.specs.alpha :as sa]
   [lift.f.functor :as f]
   [clojure.walk :as walk])
  (:import
   [clojure.lang IFn Keyword]))

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

(def movq nil)
(defmulti movq (fn [a b] [(type a) (type b)]))

(defn movq-int->reg [x r]
  (let [reg (+ 0xc0 (.indexOf r64 r))]
    [0x48 0xc7 reg (u/bsplit x 4)]))

(defmethod movq [Long Keyword] [x r]
  (movq-int->reg x r))

(defmethod movq [Integer Keyword] [x r]
  (movq-int->reg x r))

(defmethod movq [Ptr Keyword] [x r]
  (let [reg (+ 0xc0 (.indexOf r64 r))]
    [0x48 0xc7 reg (Ptr. (assoc (.-x x) :bsplit 4))]))

(defmethod movq [Keyword Ptr] [r x]
  (let [ir (.indexOf r64 r)]
    (if (< ir 8)
      (let [reg (+ 0x04 (* 8 ir))]
        [0x48 0x89 reg 0x25 (Ptr. (assoc (.-x x) :bsplit 4))])
      (let [reg (+ 0x04 (* 8 (- ir 8)))]
        [0x4c 0x89 reg 0x25 (Ptr. (assoc (.-x x) :bsplit 4))]))))

(defn reg->reg [op x y]
  (let [ix (.indexOf r64 x)
        iy (.indexOf r64 y)]
    (cond (and (< ix 8) (< iy 8))
          [0x48 op (+ 0xc0 (* 8 ix) iy)]
          (and (< ix 8) (>= iy 8))
          [0x49 op (+ 0xc0 (* 8 ix) (- iy 8))]
          (and (>= ix 8) (< iy 8))
          [0x4c op (+ 0xc0 (* 8 (- ix 8)) iy)]
          (and (>= ix 8) (>= iy 8))
          [0x4d op (+ 0xc0 (* 8 (- ix 8)) (- iy 8))])))

;; put contents of x in r
(defmethod movq [Keyword Keyword] [r x]
  (reg->reg 0x89 r x))

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
  [(movq code :rbx)
   ;; movl %ebx code
   (movq 1 :rax)
   ;; movl %eax 1
   0xcd  0x80])

(defn read [ptr len]
  [(movq 0   :rax)
   ;; clear rax
   (movq len :rdx)
   (movq ptr :rcx)
   (movq 0   :rbx) ;; stdin
   (movq 3   :rax) ;; sys_read
   0xcd  0x80])

(defn print [ptr len]
  [(movq 1 :rdx #_:rax)
   ;; movl %edx len 14
   (movq ptr  :rcx)
   ;; movl %ecx ptr to Hello, world!\n
   (movq 1    :rbx)
   ;; movl %ebx 1 stdout
   (movq 4    :rax)
   ;; movl %eax 4 sys_write
   0xcd  0x80])

;;: calling order rdi, rsi, rdx, rcx, r8, r9
;;: sys_write rdi

;; prologue :: X86 ()
;; prologue = do
;;   push rbp
;;   mov rbp rsp

;; epilogue :: X86 ()
;; epilogue = do
;;   pop rax
;;   mov rsp rbp
;;   pop rbp
;;   ret
(defmulti addq (fn [x y] [(type x) (type y)]))

(defmethod addq [Keyword Keyword] [x y]
  (reg->reg 0x01 x y))

(defn add [ptr x y]
  [(movq x :rax)
   (movq y :rdi)
   (addq :rdi :rax)
   (movq 1 :rdi)
   (addq :rdi :rax)
   [0x48 0x83 0xc0 0x30] ;; addq 0x30 rax for ascii
   (movq :rax ptr) ;; move :rax to memory
   ])

;; ret :: X86 ()
;; ret = do
;;   emit [0xc3]
;; TODO: `ret`urn

;; (fn fib [x]
;;   (if (= x 0)
;;     1
;;     (if (= x 1)
;;       1
;;       (+ (fib (- x 1))
;;          (fib (- x 2))))))

(def fib'
  '(fn fib [x]
     (if (= x 0)
       1
       (if (= x 1)
         1
         (+ (fib (- x 1))
            (fib (- x 2)))))))

(def labels
  (for [a (map char (range 97 123))
        i (rest (take 2 (range)))]
    (symbol (str a i))))

;;: Abstract Machine Spec
;;; =====================
;;;
;;; Much of this is for simplicity/ease of implementation
;;;
;;; * all arguments to functions are pointers to memory
;;; * all arguments to functions are going to be in the first n registers
;;;   - there will be no functions with arity > 3 (for the moment)

(def r64-args
  [:rdi :rsi :rdx :rcx :r8 :r9])

(defn prologue [n]
  []
  #_(loop [args r64-args
         n n
         out []]
    (if (> n 0)
      (recur (rest args)
             (dec n)
             (into out [[:push (first args)] [:mov (first args) :rsp]]))
      out)))

(defn epilogue [n]
  [])

(declare decurse)

(defmacro let-state
  {:style/indent :defn}
  [state bindings expr]
  (let [first-sym (gensym)]
    (loop [bindings (->> bindings
                         (partition 2)
                         (mapv (fn [x] [(gensym) x])))
           prevstate first-sym
           out []]
      (if (seq bindings)
        (let [[[sym [binding expr]] & more] bindings]
          (recur
           more
           sym
           (into out
                 [[binding sym] `(~(first expr) ~prevstate ~@(next expr))])))
        `(let ~(into [first-sym state] out)
           ~[expr (second (last (butlast out)))])))))

;; (defn map-state [state f xs]
;;   (if xs
;;     (let-state state
;;       [head (f (first xs))
;;        tail (map-state f (next xs))]
;;       (cons head tail))
;;     [() state]))

(defprotocol Monad
  (m-return [_ v])
  (m-bind   [t f]))

(deftype State [f]
  IFn
  (invoke [_ x] (f x))
  Monad
  (m-return [_ v]
    (State. (fn [s] [v s])))
  (m-bind   [m f]
    (State. (fn [s]
              (let [[v s'] (m s)
                    m2     (f v)]
                (m2 s'))))))

(s/def ::m-do-binding
  (s/and vector? (s/cat :binding ::sa/binding-form :<- #{'<-} :expr any?)))

(defmacro m-do [& exprs]
  (let [steps  (->> exprs
                    (map #(s/conform ::m-do-binding %))
                    (remove s/invalid?)
                    (map (juxt (comp #(s/unform ::sa/binding-form %) :binding)
                               :expr))
                    (reverse))
        exprs' (remove #(s/valid? ::m-do-binding %) exprs)
        result (reduce (fn [expr [sym mv]]
                         `(m-bind ~mv
                                  (fn [~sym] ~expr)))
                       (last exprs')
                       steps)]
    result))

(defn empty [t]
  (let [ctor (resolve (symbol (str "->" (name t))))]
    (->> ctor meta :arglists first (map (constantly nil)) (apply ctor))))

(defmacro defm [name arglist -> t expr]
  (if (= -> '->)
    `(defn ~name ~arglist
       ~(walk/postwalk (fn [expr]
                         (if (seq? expr)
                           (case (first expr)
                             return `(m-return (empty '~t) ~(second expr))
                             do     `(m-do ~@(rest expr))
                             expr)
                           expr))
                       expr))
    (throw (Exception. "Invalid syntax: ->"))))

(defn get
  []
  (State. (fn [s] [s s])))

(defn put
  [s]
  (State. (fn [_] [nil s])))

(defn modify
  [f & args]
  (State. (fn [s] (let [s' (apply f s args)] [s' s']))))

(defn run-state [m s]
  (m s))

(defn eval-state [m s]
  (first (m s)))

(defn exec-state [m s]
  (second (m s)))

(defm label [] -> State
  (do [{[l] :labels} <- (get)]
      [_ <- (modify update :labels rest)]
      (return l)))

(defm extend [sym] -> State
  (do [{[l] :labels} <- (get)]
      [_ <- (modify #(-> %
                         (update :labels rest)
                         (assoc-in [:vars sym] l)))]
      (return l)))

(defm cons-m [x xs] -> State
  (do [x' <- x]
      [xs' <- xs]
      (return (cons x' xs'))))

(defm map-state [f xs] -> State
  (if xs
    (cons-m (f (first xs)) (map-state f (next xs)))
    (return ())))

(defm call-fn
  ;; TODO: this is the actual fn call, so we should be able to prep args
  ;; here. We need to eval the args, then load the results into registers
  ;; or push them onto the stack
  [op args] -> State
  (do [op'   <- (decurse op)]
      [args' <- (map-state decurse args)]
      (let [arity (count args)]
        (return (concat
                 (prologue arity)
                 (cons op' args')
                 (epilogue arity))))))

(defm make-lambda [name [arg1] expr] -> State
  (do [_     <- (modify assoc-in [:lex arg1] :rdi)]
      [name' <- (extend name)]
      [expr' <- (decurse expr)]
      (return (cons [:label name'] expr'))))

(defm if-then-else [test consequent alternative] -> State
  (do [label'       <- (label)]
      [test'        <- (decurse test)]
      [consequent'  <- (decurse consequent)]
      [alternative' <- (decurse alternative)]
      (return [test' [:jne label'] consequent' [:label label'] alternative'])))

(defm lookup-lexical-values [sym] -> State
  (do [env <- (get)]
      (return (when-let [reg (get-in env [:lex sym])]
                ;; TODO: what do we do here? we're trying to use a fn binding
                ;; we know its register - or something
                ;; do something with reg
                [[:mov reg :rax]]))))

(defm lookup-function [sym] -> State
  (do [env <- (get)]
      (if-let [label (get (:vars env) sym)]
        ;; TODO: here, we have a function call - we don't know any more
        ;; this jumps to the function code, but what about args/etc not enough
        ;; info this needs to go back up the stack to be intepreted there... or
        ;; does it?
        ;; load registers
        (return [[:jmp label]])
        (throw (Exception. (format "Unknown symbol: %s" (name sym)))))))

(defm decurse [form] -> State
  (cond
    (seq? form)
    (let [[op & args] form]
      (case op
        if
        (apply if-then-else args)
        fn
        (apply make-lambda args)
        ;; else
        (call-fn op args)))
    (symbol? form)
    (or (lookup-lexical-values form)
        (lookup-function form))
    :else
    (return form)))

(eval-state (decurse fib') {:labels labels :vars {'+ '+ '= '= '- '-}})

;; (map fib (range 0 10))

(defn buffer [n]
  (repeat n 0))
;; ^^ to-bytes

(def program
  (let [[ptr len] (alloc! (buffer 32))]
    (flatten
     [;;(read ptr len)
      (add ptr 3 5)
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
