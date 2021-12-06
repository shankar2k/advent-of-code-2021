;;;; Header

(setq filename "input.txt")

(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))

;;;; Task Functions

(defun day01 (&optional part2 file)
  (cl-loop for ddata on (aoc-input file 'int)
           count (or (and (not part2) (>= (length ddata) 2)
                          (seq-let (a b) ddata (> b a)))
                     (and part2 (>= (length ddata) 4)
                          (seq-let (a _ _ d) ddata (> d a))))))

;; (day01) 
;;  ===> 1655

;; (day01 t) 
;;  ===> 1683

;;;; Old Versions

(defun day01-part2-old (&optional file)
  (let ((sum-integers (cl-loop for ddata on (aoc-input file 'int)
                               when (>= (length ddata) 3)
                               collect (seq-let (a b c) ddata (+ a b c)))))
    (cl-loop for sdata on sum-integers
             count (and (>= (length sdata) 2)
                        (seq-let (a b) sdata (> b a))))))
