;;;; Header

(setq filename "input.txt"
      max-lisp-eval-depth 2000
      max-specpdl-size 10000)


(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))

;;;; Task Functions

(defun flip-point (p n)
  (min p (- (* 2 n) p)))

(defun fold-point (coord fold)
  (seq-let (dim n) fold
    (seq-let (x y) coord
        (if (equal dim "x")
            (list (flip-point x n) y)
          (list x (flip-point y n))))))

(defun get-paper-dims (table)
  (mapcar #'1+ (apply #'sbr-seq-max (ht-keys table))))


(defun fold-along (coord-table fold)
  (let ((folded-points (ht-map (lambda (k _v) (fold-point k fold))
                               coord-table)))
    (ht<-alist (sbr-pairlis folded-points t))))

(defun print-coords (table)
  (seq-let (maxx maxy) (get-paper-dims table)
    (cl-loop for y from 0 below maxy
             concat (concat (cl-loop for x from 0 below maxx
                                     concat (if (ht-get table (list x y))
                                                "#"
                                              "."))
                            "\n"))))

(defun print-table (prefix table)
  (format "%s [%d] %S:\n%s" prefix (ht-size table)
          (get-paper-dims table) (print-coords table)))

(defun day13 (&optional file part2 debug)
  (seq-let (coord-list folds)
      (aoc-input file '(blank-line
                        (newline "%d,%d")
                        (newline "fold along %s=%d")))
    (let ((coord-table (ht<-alist (sbr-pairlis coord-list t))))
      (debugmsg "%s" (print-table "Initial points" coord-table))
      (cl-loop for fold in folds
               for i from 1 to (if part2 (length folds) 1)
               do
               (setq coord-table (fold-along coord-table fold))
               (debugmsg (print-table (format "After Fold #%d %s = %s"
                                              i (car fold) (cadr fold))
                                      coord-table))
               finally return (if part2
                                  (print-table "Final Result" coord-table)
                                (ht-size coord-table))))))

;;;; Old Task Functions

(defun process-instructions (&optional file)
  (let* ((data (aoc-input file '(blank-line)))
         (coord-list (cl-loop for line in (s-lines (s-chomp (car data)))
                          collect (str2num (s-split "," line))))
         (coord-table (ht<-alist (sbr-pairlis coord-list t)))
         (folds (cl-loop for line in (s-lines (cadr data))
                         collect (seq-let (dim n)
                                     (cddr (s-split "[ =]" line))
                                   (list dim (str2num n))))))
    (cons coord-table folds)))

(defun process-instructions-old (&optional file)
  (let* ((data (aoc-input file '(blank-line)))
         (coords-text (split-string (s-chomp (car data)) "\n"))
         (coords (cl-loop for line in coords-text
                          collect (str2num (split-string line ","))))
         (folds-text (split-string (cadr data) "\n"))
         (folds (cl-loop for line in folds-text
                         collect (cddr (split-string line "[ =]")))))
    (cons coords folds)))

(defun fold-point-old (coord fold)
  (seq-let (dim n) fold
    (seq-let (x y) coord
      (let ((n (str2num n)))
        (if (equal dim "x")
            (list (flip-point x n) y)
          (list x (flip-point y n)))))))

(defun fold-along-old (coord-table fold)
  (cl-loop for coord hash-keys of coord-table
           with point-table = (make-hash-table :test #'equal)
           do (ht-set point-table (fold-point-old coord fold) t)
           finally return point-table))

(defun print-coords-old (table)
  (seq-let (maxx maxy) (get-paper-dims table)
    (cl-loop for y from 0 below maxy
             concat (concat (cl-loop for x from 0 below maxx
                                     if (gethash (list x y) table)
                                     concat "#"
                                     else concat ".") "\n"))))

(defun day13-old (&optional file instr-count debug)
  (let* ((pdata (process-instructions-old file))
         (coords (car pdata))
         (coord-table (cl-loop for coord in coords
                               with init-table = (make-hash-table :test #'equal)
                               do (puthash coord t init-table)
                               finally return init-table))
         (folds (cdr pdata)))
    (debugmsg "Initial points [%d] %S:\n%s"
              (hash-table-count coord-table)
              (get-paper-dims coord-table)
              (print-coords coord-table))
    (cl-loop for fold in folds
             for i from 1 to (or instr-count (length folds))
             do
             (setq coord-table (fold-along-old coord-table fold))
             (debugmsg "After Fold #%d %s = %s [%d] %S \n%s"
                       i (car fold) (cadr fold)
                       (hash-table-count coord-table)
                       (get-paper-dims coord-table)
                       (print-coords-old coord-table))
             finally return (hash-table-count coord-table)))) 
