;;;; Header

(setq filename "input.txt")

(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))

;;;; Task Functions

(defun day08-part1 (&optional file)
  (let* ((data (aoc-input file)))
    (cl-loop for line in data
             sum (let* ((raw-output (cadr (split-string line " | ")))
                        (output (split-string raw-output " ")))
                        (cl-loop for digit in output
                          count (member (length digit) '(2 3 4 7)))))))


(defun put-digit-hash (num digit dict)
  (puthash num digit dict)
  (puthash (cl-sort digit #'<) num dict))


(defun deduce-digits (raw-input &optional debug)
  (let ((dict (make-hash-table :test #'equal))
        (input   (split-string raw-input " "))
        (input2  nil)
        (input3  nil))
    (dolist (digit input)
      (cl-case (length digit)
        (2 (put-digit-hash 1 digit dict))
        (3 (put-digit-hash 7 digit dict))
        (4 (put-digit-hash 4 digit dict))
        (7 (put-digit-hash 8 digit dict))
        (t (push digit input2))))
    (dolist (digit input2)
      (cond ((and (= (length digit) 5) (seq-is-subset (gethash 1 dict) digit))
             (put-digit-hash 3 digit dict))
            ((and (= (length digit) 6)
                  (not (seq-is-subset (gethash 1 dict) digit)))
             (put-digit-hash 6 digit dict))
            ((and (= (length digit) 6) (seq-is-subset (gethash 4 dict) digit))
             (put-digit-hash 9 digit dict))
            ((and (= (length digit) 6)
                  (not (seq-is-subset (gethash 4 dict) digit)))
             (put-digit-hash 0 digit dict))
            (t (push digit input3))))
    (dolist (digit input3)
      (cond ((and (= (length digit) 5)
                  (not (seq-is-subset digit (gethash 9 dict))))
             (put-digit-hash 2 digit dict))
            ((and (= (length digit) 5) (seq-is-subset digit (gethash 9 dict)))
             (put-digit-hash 5 digit dict))))
    (when debug
      (maphash (lambda (key val) (message "%S --> %S" key val)) dict))
    dict))

(defun day08-part2 (&optional file debug)
  (let* ((data (aoc-input file)))
    (cl-loop for line in data
             sum (seq-let (raw-input raw-output) (split-string line " | " )
                   (cl-loop for digit in (split-string raw-output " ")
                            with mapping = (deduce-digits raw-input debug)
                            sum (let ((value (gethash (cl-sort digit #'<)
                                                      mapping)))
                                  (+ (* 9 total) value)) into total
                            finally return (progn
                                             (when debug
                                               (message "output %s => %d"
                                                        raw-output total))
                                             total))))))

;; (day08-part1)
;;  ===> 352

;; (day08-part2)
;;  ===> 936117


;;;; Notes


;;;; Old Task Functions


(defun day08-part1-old (&optional file)
  (let* ((data (aoc-input file)))
    (cl-loop for line in data
             sum (cl-loop for digit in (split-string (cadr (split-string line " | ")) " ")
                          count (or (= (length digit) 2)      ; 1
                                    (= (length digit) 3)      ; 7
                                    (= (length digit) 4)      ; 4
                                    (= (length digit) 7)))))) ; 8
