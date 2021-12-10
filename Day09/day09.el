;;;; Header

(setq filename "input.txt")

(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))

;;;; Task Functions


(defun is-low-point (x lines)
  (seq-let (pline line nline) lines
    (and line
         (let ((curr (aref line x)))
           (and (or (= x (1- len)) (< curr (aref line (1+ x))))
                (or (= x 0)        (< curr (aref line (1- x))))
                (or (not pline)    (< curr (aref pline x)))
                (or (not nline)    (< curr (aref nline x))))))))

(defun get-low-points (data)
  (cl-loop for lines on (cons nil data)
           for y from 0 to (length data)
           append (cl-loop for x from 0 below len
                           when (is-low-point x lines)
                           collect (list (1+ (- (aref (cadr lines) x) ?0))
                                         x y))))

(defun day09-part1 (&optional file)
  (let* ((data (aoc-input file))
         (len (length (car data)))
         (low-points (get-low-points data)))
    (cons low-points
          (cl-loop for pt in low-points
                   sum (car pt)))))


(defun day09-part2 (&optional file debug)
  (let* ((data (aoc-input file))
         (height (length data))
         (len (length (car data)))
         (grid (make-vector2d height len 0))
         (low-points (car (day09-part1 file)))
         (point-count (length low-points))
         (basin-counts (make-vector point-count 1)))
    ;; init with low points
    (cl-loop for (_score x y) elements of low-points using (index ind)
             do (setf (aref2d grid x y) (1+ ind)))
    ;; put in nines
    (cl-loop for line elements of data using (index y)
             do (cl-loop for x from 0 below len
                         when (= (aref line x) ?9)
                         do (setf (aref2d grid x y) -1)))
    (when debug
      (message "Grid Before: %S" grid))
    ;; propagate and count basin sizes
    (setq found-zero t)
    (while found-zero
      (setq found-zero nil)
      (cl-loop for y from 0 below height
               do (cl-loop for x from 0 below len
                           when (zerop (aref2d grid x y))
                           do (let* ((left  (when (> x 0) (aref2d grid (1- x) y)))
                                     (right (when (< x (1- len)) (aref2d grid (1+ x) y)))
                                     (up    (when (> y 0) (aref2d grid x (1- y))))
                                     (down  (when (< y (1- height)) (aref2d grid x (1+ y))))
                                     (bestind (seq-max (delq nil (list left right up down)))))
                                (setq found-zero t)
                                (when (> bestind 0)
                                  (setf (aref2d grid x y) bestind)
                                  (cl-incf (aref basin-counts (1- bestind)))))))
      (when debug
        (message "Grid: %S" grid)))
    (when debug
      (message "Basin Count: %S" basin-counts))
    ;; get three largest
    (sort basin-counts #'>)
    (seq-let (a b c) basin-counts
      (cons (list a b c) (* a b c)))))


;;;; Notes


;;;; Old Task Functions

(defun day09-part1-old (&optional file)
  (let* ((data (aoc-input file))
         (len (length (car data)))
         (low-points (cl-loop for lines on (cons nil data)
                              for y from 0 to (length data)
                              append (seq-let (pline line nline) lines
                                       (cl-loop for n from 0 below len
                                                when (and line
                                                          (let ((curr (aref line n)))
                                                            (and (or (= n (1- len)) (< curr (aref line (1+ n))))
                                                                 (or (= n 0)        (< curr (aref line (1- n))))
                                                                 (or (not pline)    (< curr (aref pline n)))
                                                                 (or (not nline)    (< curr (aref nline n))))))
                                                collect (list (1+ (- (aref line n) ?0)) n y))))))
    (cons low-points
          (cl-loop for pt in low-points
                   sum (car pt)))))


(defun day09-part2-old (&optional file)
  (let* ((data (aoc-input file))
         (height (length data))
         (len (length (car data)))
         (grid (seq-into (cl-loop for n below height
                                  collect (make-vector len 0)) 'vector))
         (low-points (car (day09-part1-old file)))
         (point-count (length low-points))
         ;; SBR: basin-counts can be initialized to one if counting during
         ;; propagation
         (basin-counts (make-vector point-count 0)))
    ;; init with low points
    (cl-loop for pt-score in low-points
             for ind from 1 to point-count
             do
             (seq-let (score x y) pt-score
               (setf (aref (aref grid y) x) ind)))
    ;; put in nines
    (cl-loop for line in data
             for y from 0 below height
             do (cl-loop for x from 0 below len
                         when (= (aref line x) ?9)
                         do (setf (aref (aref grid y) x) -1)))
    (message "Grid Before: %S" grid)
    ;; propagate
    (setq found-zero t)
    (while found-zero
      (setq found-zero nil)
      (cl-loop for y from 0 below height
               do (cl-loop for x from 0 below len
                           when (zerop (aref (aref grid y) x))
                           do
                           (setq found-zero t)
                           (let* ((left  (if (> x 0)           (aref (aref grid y) (1- x)) -1))
                                  (right (if (< x (1- len))    (aref (aref grid y) (1+ x)) -1))
                                  (up    (if (> y 0)           (aref (aref grid (1- y)) x) -1))
                                  (down  (if (< y (1- height)) (aref (aref grid (1+ y)) x) -1))
                                  (bestind (max left right up down)))
                             (when (> bestind 0)
                               (setf (aref (aref grid y) x) bestind)))))
      (message "Grid: %S" grid))
    ;; count basin size
    ;; SBR: this can be done while doing propagation
    (cl-loop for y from 0 below height
             do (cl-loop for x from 0 below len
                         do (let ((basin (aref (aref grid y) x)))
                              (when (> basin 0)
                                (cl-incf (aref basin-counts (1- basin)))))))
    (message "Basin Count: %S" basin-counts)
    ;; get three largest
    ;; SBR: why didn't I just sort the basin-counts?
    (let ((largest (cl-loop for n below 3
                            collect
                            (cl-loop for b across basin-counts 
                                     for i from 0 below point-count
                                     with max = 0
                                     with ind = -1
                                     when (> b max)
                                     do
                                     (setq max b)
                                     (setq ind i)
                                     finally return (progn (setf (aref basin-counts ind) -1) max)))))
      (cl-loop for m in largest with product = 1
               do (setq product (* product m))
               finally return (cons largest product)))))

