;;;; Header

(setq filename "input.txt")

(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))

;;;; Task Functions


(defun fuel (data n part2)
  (cl-loop for pos in data
           sum (let ((diff (abs (- n pos))))
                 (if part2
                     (/ (* diff (1+ diff)) 2)
                     diff))))



(defun day07 (&optional part2 file)
  (let* ((data (aoc-input file 'int ",")))
    (cl-loop for n from 0 to (apply #'max data)
             minimize (let ((costn (fuel data n part2)))
                        (message "%d: %d" n costn)
                        costn))))

;;;; Notes


;;;; Old Task Functions


