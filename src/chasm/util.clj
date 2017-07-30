(ns chasm.util)

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
