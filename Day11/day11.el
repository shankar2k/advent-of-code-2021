;;;; Header

(setq filename "input.txt"
      max-lisp-eval-depth 2000
      max-specpdl-size 10000)


(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))

;;;; Task Functions

(defun get-grid (data)
  (let* ((height (length data))
         (width (length (car data))))
    (vconcat (cl-loop for line in data
                      collect (vconcat (seq-map (lambda (x) (- x ?0))
                                                (seq-into line 'list)))))))

(defun print-grid (grid)
  (cl-loop for line across grid
           concat (format "[ %s ]\n"
                          (cl-loop for pix across line
                                   concat (format "%2d " pix)))))

(defun increment-grid (grid)
  (cl-loop for line across grid
           do (cl-loop for pixel across-ref line
                       do (cl-incf pixel))))

(defun flash-grid (grid n &optional debug )
  (let ((flash-count
         (cl-loop for line elements of grid using (index y)
                  sum (cl-loop for pixel elements of line using (index x)
                               when (> pixel 9)
                               sum (flash-pixel grid x y)))))
    (debugmsg "After iter %d (%d):\n%s\n" n flash-count (print-grid grid))
    flash-count))


(defun flash-neighbor (grid nx ny width height)
  (when (and (<= 0 nx (1- width))
             (<= 0 ny (1- height))
             (not (zerop (aref2d grid nx ny)))
             (> (cl-incf (aref2d grid nx ny)) 9))
        (flash-pixel grid nx ny)))

(defun flash-pixel (grid x y)
  (setf (aref2d grid x y) 0)
  (1+ (cl-loop for (dx . dy) in '((-1 . -1) (0 . -1) (1 . -1)
                                  (-1 .  0)          (1 .  0)
                                  (-1 .  1) (0 .  1) (1 .  1))
               with height = (length grid)
               with width = (length (aref grid 0))
               sum (or (flash-neighbor grid (+ x dx) (+ y dy)
                                       width height) 0))))


(defun day11-part1 (&optional file count debug)
  (let* ((data (aoc-input file))
         (grid (get-grid data)))
    (debugmsg "Initial:\n%s\n" (print-grid grid))
    (cl-loop for n from 1 to count
      do
      (increment-grid grid)
      (debugmsg "After inc %d:\n%s\n" n (print-grid grid))
      sum (flash-grid grid n debug))))

(defun day11-part2 (&optional file maxcount debug)
  (let* ((data (aoc-input file))
         (grid (get-grid data))
         (pixel-count (* (length data) (length (car data))))
         (flash-count 0)
         (n 0))
    (debugmsg "Initial:\n%s\n" (print-grid grid))
    (while (and (not (= flash-count pixel-count)) (< n maxcount))
      (cl-incf n)
      (increment-grid grid)
      (setq flash-count (flash-grid grid n debug)))
    n))




;;;; Notes


;;;; Old Task Functions
