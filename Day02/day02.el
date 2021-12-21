;;;; Header

(setq filename "input.txt")

(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))

;;;; Task Functions

(defun day02 (&optional part2 file)
  (cl-loop for (command step) in (aoc-input file '(newline "%s %d"))
           with depth = 0 with pos = 0 with aim = 0
           do (pcase command
                ("forward"
                 (cl-incf pos step)
                 (when part2
                   (cl-incf depth (* aim step))))
                ("up" (cl-decf (if part2 aim depth) step))
                ("down" (cl-incf (if part2 aim depth) step)))
           finally return  (format "pos=%d, depth=%d, result=%d" pos depth (* pos depth))))

(defun day02-v2 (&optional part2 file)
  (cl-loop for line in (aoc-input file) with depth = 0 with pos = 0 with aim = 0
           do (let* ((command (split-string line " "))
                     (step (string-to-number (cadr command))))
                (pcase (car command)
                  ("forward"
                   (cl-incf pos step)
                   (when part2
                     (cl-incf depth (* aim step))))
                  ("up" (cl-decf (if part2 aim depth) step))
                  ("down" (cl-incf (if part2 aim depth) step))))
           finally return  (format "pos=%d, depth=%d, result=%d" pos depth (* pos depth))))

;; (day02) 
;;  ===> "pos=1927, depth=1091, result=2102357"

;; (day02 t) 
;;  ===> "pos=1927, depth=1090312, result=2101031224"


;;;; Old Versions

(defun day02-part1-old (&optional file)
  (cl-loop for line in (aoc-input file 'string) with depth = 0 with pos = 0
           do (let* ((command (split-string line " "))
                     (step (string-to-number (cadr command))))
                (pcase (car command)
                  ("forward" (cl-incf pos step))
                  ("up" (cl-decf depth step))
                  ("down" (cl-incf depth step))))
           finally return  (format "pos=%d, depth=%d, result=%d" pos depth (* pos depth))))

(defun day02-part2-old (&optional file)
  (cl-loop for line in (aoc-input file 'string)
           with depth = 0 with pos = 0 with aim = 0
           do (let* ((command (split-string line " "))
                     (step (string-to-number (cadr command))))
                (pcase (car command)
                  ("forward"
                   (cl-incf pos step)
                   (cl-incf depth (* aim step)))
                  ("up" (cl-decf aim step))
                  ("down" (cl-incf aim step))))
           finally return (format "pos=%d, depth=%d, aim=%d, result=%d" pos depth aim (* pos depth))))
