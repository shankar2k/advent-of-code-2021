;;;; Header

(setq lexical-binding t
      max-lisp-eval-depth 2000
      max-specpdl-size 10000)

(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))

;;;; Task Functions

(defun target-score (x y bounds)
  (seq-let (xmin xmax ymin ymax) bounds
      (cons
       (cond ((< x xmin) -1)
             ((> x xmax) 1)
             (t 0))
       (cond ((< y ymin) -1)
             ((> y ymax) 1)
             (t 0)))))

(defun sort-vels (v1 v2)
  (or (< (car v1) (car v2))
      (and (= (car v1) (car v2))
           (< (cadr v1) (cadr v2)))))

(defun launch-probe (vx vy iter-count bounds &optional debug)
  (defun get-result ()
    (let ((hit (equal score '(0 . 0))))
      (debugmsg "Iter %02d: x=%d y=%d vx=%d vy=%d ymax=%d %S %s the target!"
                n x y vx vy ymax score
                (if hit "Hit" "Missed"))
      (list ymax hit n x y)))
  (cl-loop for n from 1 to iter-count with x = 0 with y = 0
           do
           (cl-incf x vx) ; velocity
           (cl-incf y vy) ; velocity
           (cl-decf vy) ; gravity
           (unless (zerop vx) ; drag
             (if (> vx 0)
                 (cl-decf vx)
               (cl-incf vx)))
           (setq score (target-score x y bounds))
           maximize y into ymax
           when (or (equal score '(0 . 0))
                    (and (< y (nth 2 bounds)) (<= vy 0))
                    (and (= vx 0) (/= (car score) 0)))
           return (get-result)
           finally return (get-result)))

(defun day17 (&optional file vgrid debug)
  (defun day17-debug ()
    (debugmsg "vx=%d vy=%d: ymax=%S hit-count=%d fail-count=%d result=%S"
              vx vy ymax hit-count fail-count result))
  (let ((bounds (aoc-input
                 file
                 "target area: x=%d..%d, y=%d..%d"))
        (iter-count 500)
        (vgrid (or vgrid '(-200 200 -200 200)))
        (hit-count 0)
        (hit-list nil))
    (seq-let (vxmin vxmax vymin vymax) vgrid
      (cl-loop for vy from vymin to vymax
             maximize (cl-loop for vx from vxmin to vxmax with fail-count = 0
                               do
                               (setq result (launch-probe vx vy iter-count
                                                          bounds debug))
                               (seq-let (_ hit n) result
                                 (when (= n iter-count)
                                   (cl-incf fail-count))
                                 (when hit
                                   (cl-incf hit-count)
                                   (push (list vx vy) hit-list)))
                               when (cadr result) ; hit
                               maximize (car result) into ymax
                               do (day17-debug)
                               finally return (or ymax -1000000)) into besty
             finally return (progn
                              (setq sbr-hit-list (sort hit-list #'sort-vels))
                              (list besty hit-count))))))

;;;; notes

;;;; Old Task Functions

(defun launch-probe-old (vx vy iter-count bounds &optional debug)
  (defun get-result ()
    (let ((hit (equal score '(0 . 0))))
      (debugmsg "Iter %02d: x=%d y=%d vx=%d vy=%d ymax=%d %S %s the target!"
                n x y vx vy ymax score
                (if hit "Hit" "Missed"))
      (list ymax hit n x y)))
  (cl-loop for n from 1 to iter-count with x = 0 with y = 0
           do
           (cl-incf x vx)
           (cl-incf y vy)
           (cl-decf vy)
           (unless (zerop vx)
             (if (> vx 0)
                 (cl-decf vx)
               (cl-incf vx)))
           (setq score (target-score x y bounds))
           maximize y into ymax
           when (or (equal score '(0 . 0))
                    (and (< y (nth 2 bounds)) (<= vy 0)))
           return (get-result)
           finally return (get-result)))


(defun day17-old (&optional file vgrid debug)
  (let ((bounds (aoc-input file
                           "target area: x=%d..%d, y=%d..%d"))
        (iter-count 500)
        (vgrid (or vgrid '(-200 200 -200 200))))
    (seq-let (vxmin vxmax vymin vymax) vgrid
      (setq hit-count 0)
      (setq hit-list nil)
      (cl-loop for vy from vymin to vymax
             maximize (cl-loop for vx from vxmin to vxmax with fail-count = 0
                               do
                               (setq result (launch-probe-old vx vy iter-count
                                                              bounds debug))
                               (seq-let (_ hit n) result
                                 (when (= n iter-count)
                                   (cl-incf fail-count))
                                 (when hit
                                   (cl-incf hit-count)
                                   (push (list vx vy) hit-list)))
                               when (cadr result) ; hit
                               maximize (car result) into ymax
                               do (debugmsg
                                   (concat "vx=%d vy=%d: ymax=%S hit-count=%d "
                                           "fail-count=%d result=%S")
                                   vx vy ymax hit-count fail-count result)
                               finally return (or ymax -1000000)) into besty
             finally return (progn
                              (setq sbr-hit-list (sort hit-list #'sort-vels))
                              (list besty hit-count))))))
