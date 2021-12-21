;;;; Header

(setq lexical-binding t
      max-lisp-eval-depth 8000
      max-specpdl-size 20000)

(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))

;;;; Task Functions

(defun vector-size (vec)
  (when (vectorp vec)
    (cons (length vec) (vector-size (aref vec 0)))))

(defun vector-stack (vec)
  (cl-loop for x across vec
           vconcat x))

(defun sub-vector2d (vec2d x1 y1 x2 y2)
  (vconcat
   (cl-loop for y from y1 below y2
            collect (seq-subseq (aref vec2d y) x1 x2))))

(defun set-subvector2d (input-vec2d sub-vec2d x1 y1)
  (seq-let (height width) (vector-size input-vec2d)
    (seq-let (subh subw) (vector-size sub-vec2d)
      (let ((y2 (+ y1 subh))
            (x2 (+ x1 subw)))
      (when (and (<= y2 height) (<= x2 width))
        (cl-loop for iy from y1 below y2
                 for sy from 0 below subh
                 do (cl-loop for ix from x1 below x2
                             for sx from 0 below subw
                             do (setf (aref2d input-vec2d ix iy)
                                      (aref2d sub-vec2d sx sy)))
                 finally return input-vec2d))))))

(defun print-image (image)
  (string-join (seq-map #'concat image) "\n"))

(defun count-image (image ch)
  (cl-loop for row across image
           sum (seq-count (lambda (p) (eq p ch)) row)))

(defun dilate-image-border (image size init)
  (let* ((old-size (vector-size image))
         (new-size (seq-map (lambda (x) (+ x (* 2 size))) old-size))
         (new-image (make-vector2d (car new-size) (cadr new-size) init)))
    (set-subvector2d new-image image size size)))

(defun pixel2bin (p)
  (if (eq p ?#) ?1 ?0))

(defun get-recipe-index (recipe window)
  (aref recipe
        (str2num (concat (seq-map #'pixel2bin (vector-stack window))) 2)))

(defun apply-recipe (recipe image border)
  (seq-let (height width) (vector-size image)
    (let* ((pad-image (dilate-image-border image 2 border))
           (new-border (get-recipe-index recipe (make-vector2d 3 3 border)))
           (result
            (cl-loop for ny from 0 below (+ 2 height)
                     vconcat
                     (list
                      (cl-loop for nx from 0 below (+ 2 width)
                               vconcat
                               (list
                                (get-recipe-index
                                 recipe (sub-vector2d pad-image nx ny
                                                      (+ 3 nx) (+ 3 ny)))))))))
      (list result new-border))))

(defun day20 (&optional file iter-count debug)
  (let* ((data (aoc-input file
                          '(blank-line ("" "%c")
                                       (newline ("" "%c")))
                          'vector))
         (recipe (aref data 0))
         (result (list (aref data 1) ?.)))
    (dotimes (n iter-count (count-image (car result) ?#))
      (setq result (apply #'apply-recipe recipe result))
      (seq-let (image border) result
        (debugmsg "After %d (count=%d, new border=%c):\n%s\n"
                  n (count-image image ?#) border (print-image image))))))

;;;; Notes

;;;; Old Task Functions
