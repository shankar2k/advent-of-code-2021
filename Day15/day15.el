;;;; Header

(setq max-lisp-eval-depth 2000
      max-specpdl-size 10000)


(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))
;; Download from heap.el http://www.dr-qubit.org/emacs.php
(require 'heap (expand-file-name "../heap.el"))

;;;; Task Functions

(defun get-risk (risk rx ry rwidth rheight)
  (let* ((mx (% rx rwidth))
         (my (% ry rheight))
         (mr (+ (aref2d risk mx my) (/ rx rwidth) (/ ry rheight))))
    (1+ (% (1- mr) 9))))


(defun day15 (&optional file part2 debug)
  (let* ((risk (aoc-input file '(newline ("" "%d")) 'vector))
         (raw-height (length risk))
         (raw-width (length (elt risk 0)))
         (height (if part2 (* 5 raw-height) raw-height))
         (width (if part2 (* 5 raw-width) raw-width))
         (best-paths (make-vector2d height width nil))
         (node-heap (make-heap (lambda (node1 node2)
                                 (< (car node1) (car node2))))))

    ;; SBR: Due to a bug in heap-build, this causes an error. It can be fixed
    ;; by ensuring in heap-build that the minimize size of the initial heap is
    ;; 10, even if the initial vector is smaller.
    
    ;; (node-heap (heap-build (lambda (node1 node2)
    ;;                          (< (car node1) (car node2)))
    ;;                        (vconcat (list (list 0 0 0))))))

    (heap-add node-heap (list 0 0 0))
    (while (not (heap-empty node-heap))             
      (seq-let (val x y) (heap-delete-root node-heap)       
        (debugmsg "Processing (%d) (%d,%d)->..." val x y)
        (cl-loop for (dx dy) in `((,(1- x) ,y) (,(1+ x) ,y)
                                  (,x ,(1- y)) (,x ,(1+ y)))
                 when (and (<= 0 dx (1- width))
                           (<= 0 dy (1- height)))
                 do (let ((new-val (+ val (get-risk risk dx dy
                                                    raw-width raw-height))))
                      (unless (when-let ((best (aref2d best-paths dx dy)))
                                (<= (car best) new-val))
                        (setf (aref2d best-paths dx dy) (list new-val x y))
                        (debugmsg "Adding (%d) (%d,%d)->(%d,%d)..."
                                  new-val dx dy x y)
                        (heap-add node-heap (list new-val dx dy)))))))
    (aref2d best-paths (1- height) (1- width))))


;;;; Notes

;; day15 stores paths in a heap
;; (benchmark 1 (message "total risk --> %S" (day15 nil t)))
;; total risk --> (2995 499 498)
;; Elapsed time: 12.433278s (4.176842s in 59 GCs)


;; day15-old stores paths in a sorted list
;; (benchmark 1 (message "total risk --> %S" (day15-old nil t)))
;; total risk --> (2995 499 498)
;; Elapsed time: 472.116720s (1.118504s in 16 GCs)


;;;; Old Task Functions

(defun day15-old (&optional file part2 debug)
  (let* ((data (aoc-input file '(newline ("" "%d"))))
         (raw-height (length data))
         (raw-width (length (car data)))
         (height (if part2 (* 5 raw-height) raw-height))
         (width (if part2 (* 5 raw-width) raw-width))
         (risk (vconcat (cl-loop for line in data
                                 collect (vconcat line))))
         (best-paths (make-vector2d  height width nil))
         (node-queue (list (list 0 0 0))))
    (cl-loop while node-queue do
             (seq-let (val x y) (pop node-queue)
               (debugmsg "Processing (%d) (%d,%d)->..." val x y)
               (cl-loop for (dx dy) in `((,(1- x) ,y) (,(1+ x) ,y)
                                         (,x ,(1- y)) (,x ,(1+ y)))
                        when (and (<= 0 dx (1- width))
                                  (<= 0 dy (1- height)))
                        do (let ((new-val (+ val
                                             (get-risk risk dx dy
                                                       raw-width raw-height))))
                             (unless (when-let ((best (aref2d best-paths
                                                              dx dy)))
                                       (<= (car best) new-val))
                               (setf (aref2d best-paths dx dy)
                                     (list new-val x y))
                               (debugmsg "Adding (%d) (%d,%d)->(%d,%d)..."
                                         new-val dx dy x y)
                               (push (list new-val dx dy) node-queue)))
                        (setq node-queue (sort node-queue
                                               (lambda (n1 n2)
                                                 (< (car n1) (car n2))))))))
    (aref2d best-paths (1- height) (1- width))))
