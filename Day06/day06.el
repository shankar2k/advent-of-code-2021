;;;; Header

(setq filename "input.txt")

(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))

;;;; Task Functions


(defun day06 (days &optional file debug)
  (let* ((data (aoc-input file 'int ","))
         (counts (make-list 9 0)))
    (message "Initial State: %S" data)
    (dolist (fish data)
      (cl-incf (nth fish counts)))
    (dotimes (n days (cons counts (seq-sum counts)))
      (let ((a (car counts)))
        (setq counts (nconc (cdr counts) (list a)))
        (cl-incf (nth 6 counts) a))
      (if debug
          (message "After Day %2d (%d): %S" n (seq-sum counts) counts)
        (message "After Day %2d (%d)" n (seq-sum counts))))))

;; (day06 80)
;;  ===> ((25816 42347 32726 46656 43244 46675 58165 23105 33461) . 352195)

;; (day06 256) 
;;  ===> ((141148347315 162057469235 171485519495 188293726992 209248181091 219291507258 252739136348 118233178535 137808935019) . 1600306001288)

;;;; Notes

;; [1 1 2 1 0 0 0 0 0]
;; [1 2 1 0 0 0 1 0 1]
;; [2 1 0 0 0 1 1 1 1]

;; 0 -> 1
;; n0 -> 2  (6 8)
;; n0+7 -> 3 (6 1 8)
;; n0+9 --> 4 (4 6 6 8)
;; n0+14 --> 5 (6 1 1 3 8)
;; n0+16 --> 7 (4 6 6 1 6 8 8)
;; n0+18 --> 8 (2 4 4 6 4  ...

;;;; Old Task Functions

;; SBR: Naive Version that keeps track of each fish

(defun day06-part1 (days &optional file debug)
  (let* ((data (aoc-input file 'int ",")))
    (message "Initial State: %S" data)
    (cl-loop for n from 1 to days
             do
             (setq data
                   (append data (cl-loop for fish in-ref data
                                         if (zerop fish)
                                         collect (progn (setf fish 6) 8)
                                         else do (cl-decf fish))))
             (if debug
                 (message "After Day %2d (%d): %S" n (length data) data)
               (message "After Day %2d (%d)" n (length data))))
    (length data)))
