;;;; Header

(setq filename "input.txt")

(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))

;;;; Task Functions

(defun parse-vent-data (&optional file all-coords)
  (let ((data (aoc-input file 'string)))
    (cl-loop for line in data
             do (setq coords (str2num (split-string (s-replace " -> " "," line) ",")))
             when (or all-coords (seq-let (x1 y1 x2 y2) coords
                                   (or (= x1 x2) (= y1 y2))))
             collect coords)))


(defun canon-seg (seg)
  (seq-let (x1 y1 x2 y2) seg
      (if (or (< x1 x2) (and (= x1 x2) (<= y1 y2)))
          seg
        (list x2 y2 x1 y1))))

(defun split-horiz-vert-diag (data)
  (cl-loop for seg in data
           if (seq-let (x1 y1 x2 y2) seg
                (= x1 x2))
           collect (canon-line seg) into vert
           else if (seq-let (x1 y1 x2 y2) seg
                     (= y1 y2))
           collect (canon-line seg) into horiz
           else collect seg into diag
           finally return (list horiz vert diag)))

(defun point-in-horiz (x y hc)
  (seq-let (hx1 hy1 hx2 hy2) hc
      (and (= y hy1)
           (<= (min hx1 hx2) x (max hx1 hx2)))))

(defun point-in-vert (x y vc)
  (seq-let (vx1 vy1 vx2 vy2) vc
      (and (= x vx1)
           (<= (min vy1 vy2) y (max vy1 vy2)))))

(defun point-in-diag (x y dc)
  (seq-let (dx1 dy1 dx2 dy2) dc
    (and (<= (min dx1 dx2) x (max dx1 dx2))
         (<= (min dy1 dy2) y (max dy1 dy2))
         (zerop (- (* (- dx2 dx1) (- y dy1))
                   (* (- dy2 dy1) (- x dx1)))))))

(defun count-point-overlaps (x y data)
  (seq-let (hdata vdata ddata) data
    (+ (cl-loop for h in hdata
                count (point-in-horiz x y h))
       (cl-loop for v in vdata
                count (point-in-vert x y v))
       (cl-loop for d in ddata
                count (point-in-diag x y d)))))

(defun day05 (&optional part2)
  (let* ((data (parse-vent-data nil part2))
         (sdata (split-horiz-vert-diag data)))
    (cl-loop for x from 0 to 999
             do
             (setq result-x (cl-loop for y from 0 to 999
                                     when (>= (count-point-overlaps x y sdata) 2)
                                     collect (cons x y)))
             (message "x=%d\n %S" x result-x)
             append result-x)))

;; WARNING! this brute force approach of testing every grid point with every
;; line segment works, but takes ~10 minutes to complete

;; (setq result-1 (day05))
;; (length result-1)
;;  ===> 7674

;; (setq result-2 (day05 t))
;; (length result-2)
;;  ===> 20898


;;;; Old Task Functions

;; SBR: Not needed because split-horiz-vert-diag is more general

(defun split-horiz-vert (data)
  (cl-loop for seg in data
           when (seq-let (x1 y1 x2 y2) seg
                  (= x1 x2))
           collect (canon-line seg) into vert
           when (seq-let (x1 y1 x2 y2) seg
                  (= y1 y2))
           collect (canon-line seg) into horiz
           finally return (cons horiz vert)))

;; SBR: Failed geometry-based approach which would make sensible if it were
;; computationally infeasible to just draw the dang lines! Maybe if the
;; intersects and overlaps weren't guaranteed to be integers ...

(defun horiz-vert-overlap (hseg vseg)
  (seq-let (hx1 hy1 hx2 hy2) hseg
    (seq-let (vx1 vy1 vx2 vy2) vseg
      (and (<= hx1 vx1 hx2) (<= vy1 hy1 vy2)
           (cons vx1 hy1)))))

(defun horiz-diag-overlap (hseg dseg)
  (seq-let (s1x1 s1y1 s1x2 s1y2) hseg
    (seq-let (s2x1 s2y1 s2x2 s2y2) dseg
      (let* ((m      (if (< s2y1 s2y2) 1 -1))
             (isect  (cons (* m (- s2y1 (+ s1x1 s1y1))) s1y1)))
        (and (point-in-horiz isect)
             (point-in-diag isect)
             isect)))))
           

(defun vert-diag-overlap (vseg dseg)
  (seq-let (s1x1 s1y1 s1x2 s1y2) vseg
    (seq-let (s2x1 s2y1 s2x2 s2y2) dseg
      (let* ((m      (if (< s2y1 s2y2) 1 -1))
             (isect  (cons s1x1 (* m (+ (- s1x1 s2x1) s2y1)))))
        (and (point-in-vert isect)
             (point-in-diag isect)
             isect)))))


(defun horiz-overlap (seg1 seg2)
  (seq-let (h1x1 h1y1 h1x2 h1y2) seg1
    (seq-let (h2x1 h2y1 h2x2 h2y2) seg2
      (and (= h1y1 h2y1)
           (when (or (<= h1x1 h2x1 h1x2)
                     (<= h1x1 h2x2 h1x2)
                     (<= h2x1 h1x1 h2x2)
                     (<= h2x1 h1x2 h2x2))
             (cl-loop for x from (max h1x1 h2x1) to (min h1x2 h2x2)
                      collect (cons x h1y1)))))))

(defun vert-overlap (seg1 seg2)
  (seq-let (v1x1 v1y1 v1x2 v1y2) seg1
    (seq-let (v2x1 v2y1 v2x2 v2y2) seg2
      (and (= v1x1 v2x1)
           (when (or  (<= v1y1 v2y1 v1y2)
                      (<= v1y1 v2y2 v1y2)
                      (<= v2y1 v1y1 v2y2)
                      (<= v2y1 v1y2 v2y2))
             (cl-loop for y from (max v1y1 v2y1) to (min v1y2 v2y2)
                      collect (cons v1x1 y)))))))


(defun diag-parallel-overlap (seg1 seg2)
  (seq-let (d1x1 d1y1 d1x2 d1y2) seg1
    (seq-let (d2x1 d2y1 d2x2 d2y2) seg2
      (and (eq (< d1y1 d1y2) (< d2y1 d2y2))
           (when (or  (<= d1y1 d2y1 d1y2)
                      (<= d1y1 d2y2 d1y2)
                      (<= d2y1 d1y1 d2y2)
                      (<= d2y1 d1y2 d2y2))
             (cl-loop for y from (max d1y1 d2y1) to (min d1y2 d2y2)
                      for x from (max d1x1 d2x1) to (min d1x2 d2x2)
                      collect (cons x y)))))))


(defun get-intersects (data1 data2 func)
  (cl-loop for seg1 in data1
           append (cl-loop for seg2 in data2
                           append (when-let ((result (funcall func seg1 seg2)))
                                    (list result)))))

(defun get-overlaps (data func)
  (cl-loop for segs on data
           append (cl-loop for seg2 in (cdr segs)
                           append (funcall func (car segs) seg2))))
  

(defun compare-overlaps (&optional file)
  (let* ((sdata (split-horiz-vert (parse-vent-data file)))
         (hdata (car sdata))
         (vdata (cdr sdata))
         (hv-overlaps (get-intersects hdata vdata #'horiz-vert-overlap))
          ;; (cl-loop for hseg in hdata
          ;;                      append (cl-loop for vseg in vdata
          ;;                                      append (when-let ((result (horiz-vert-overlap hseg vseg)))
          ;;                                               (list result)))))
         (hv-uniq (seq-uniq hv-overlaps))
         (hh-overlaps (get-overlaps hdata #'horiz-overlap))
         (hh-uniq (seq-uniq hh-overlaps))
         (vv-overlaps (get-overlaps vdata #'vert-overlap))
         (vv-uniq (seq-uniq vv-overlaps))
         (all-overlaps (append hv-uniq hh-uniq vv-uniq))
         (all-uniq (seq-uniq all-overlaps)))
    (message "HV overlaps: %d, unique: %d" (length hv-overlaps) (length hv-uniq))
    (message "HH overlaps: %d, unique: %d" (length hh-overlaps) (length hh-uniq))
    (message "VV overlaps: %d, unique: %d" (length vv-overlaps) (length vv-uniq))
    (message "All overlaps: %d, unique: %d" (length all-overlaps) (length all-uniq))
    (message "Overlaps: %S" all-uniq)
    (list (length hv-overlaps) (length hv-uniq) (length hh-overlaps) (length hh-uniq)
          (length vv-overlaps) (length vv-uniq) (length all-overlaps) (length all-uniq))))
