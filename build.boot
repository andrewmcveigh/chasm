(def +project+ 'chasm/chasm)
(def +version+ "0.1.0-SNAPSHOT")
(def +description+ "A Scheme-like language compiled to ASM")
(set-env! :dependencies '[[org.clojure/clojure "1.9.0-alpha17"]
                          [lift/f "0.1.0-SNAPSHOT"]]
          :source-paths #{"src"})
(task-options!
 pom {:project +project+ :version +version+ :description +description+})
(replace-task! [r repl] (comp ((get (ns-publics 'boot.user) 'cider)) r))


;;; Basic data types
;;; We need Integers, Chars, List
;;; What is a Char? - let's keep it really simple and go with byte/ascii
;;; What is a Char?
;;; If it's a byte, then it:
;;;  a) needs a location of where that byte is in memory
;;;  b) be able to read that byte
;;; How do we keep the location? - is everything a pointer?

;;; What is an Integer?

;;; What even is a type? If you disregard runtime inspection/reflection of types
;;; Then a type is nothing. It's an artifact of the type system - which may not
;;; even exist at runtime.

;;; A reference = a memory address
;;; A binding   = a named reference

;;: Definition: i8
;;; A signed integer stored as a byte

;;: Definition: i16
;;; A signed integer stored as 2 bytes

;;: Definition: i32
;;; A signed integer stored as 4 bytes

;;: Definition: i64
;;; A signed integer stored as 8 bytes

;;: Definition: Integer
;;; A whole number
;;; Disregarding architecture, a possibley negative, or infinite whole number of
;;; arbitrary precision

;;: Definition: Nat
;;; An Integer that is either Zero or positive

;;: Definition: Char
;;; An ascii encoded byte

;;: Cons Cell
;;; (1 . 2)
;;; +---+---+   +---+---+
;;; | p | p |-->|   |   |
;;; +-|-+---+   +---+---+
;;;   v

;;: Definition: List
;;; Nil | Cons a (List a)

;;: Definition: String
;;; List Char

;;: Definition: Pair
;;; Cons a b

;;: Definition: Symbol
;;; Pair String String

;;: Definition: AList
;;; List Pair

;;: Definition: Lamda
;;; Function pointer + env - capture current environment
;;; The current env _could_ be huge, so lets limit it to local environment
;;; meaning that the global env cannot change. - Any global binding is permanent
;;; or, is a mutable binding, the reference of which may change, but the binding
;;; cannot.

;;; TODO: get an interpreter working or jit repl
;;; TODO: no macros 'til repl
;;; TODO: must be able to jit/compile itself
;;; TODO: The interactive env is a MUST

;;; R: read
;;;       +-> bootstrap with clojure reader
;;; E: eval
;;;       +-> jit-compile
;;;       +-> load
;;;       +-> eval
;;; P: print
;;; L: loop

;;; If we're not manually writing this in assembly, just compling to it, the
;;; idea would be to get a small language running ASAP, that _could_ self-host.
;;; Would we start with a type-system, or write the type-system in the language?
