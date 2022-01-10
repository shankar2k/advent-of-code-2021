;;;; Task Functions

(defun day22-data (&optional file)
   (aoc-input file '(newline (" " "%s" ("," "%*c=%d..%d")))))

(defun threshold-cuboid (cuboid minr maxr)
  (seq-map (lambda (x)
             (seq-let (a b) x
               (list (max a minr) (min b maxr))))
           cuboid))

(defun day22-part1 (&optional file  debug)
  (let* ((data (day22-data file))
         (reactor (ht-create)))
    (dolist (pair data (ht-size reactor))
      (seq-let (command cuboid) pair
        (seq-let (xmin xmax ymin ymax zmin zmax)
            (apply #'append (threshold-cuboid cuboid -50 50))
          (cl-loop for x from xmin to xmax
                   do
                   (cl-loop for y from ymin  to ymax
                            do
                            (cl-loop for z from zmin to zmax
                                     do (if (equal command "on")
                                            (ht-set reactor (list x y z) t)
                                          (ht-remove reactor
                                                     (list x y z)))))))))))

(defun cuboid-size (cuboid)
  (or (seq-product (seq-mapn (lambda (r) (1+ (- (apply #'- r)))) cuboid)) 0))

(defun cuboid-intersect (cuboid1 cuboid2)
  (seq-let (x11 x12 y11 y12 z11 z12) (apply #'append cuboid1)
    (seq-let (x21 x22 y21 y22 z21 z22) (apply #'append cuboid2)
      (when (and (or (<= x11 x21 x12) (<= x21 x11 x22))
                 (or (<= y11 y21 y12) (<= y21 y11 y22))
                 (or (<= z11 z21 z12) (<= z21 z11 z22)))
        (list (list (max x11 x21) (min x12 x22))
              (list (max y11 y21) (min y12 y22))
              (list (max z11 z21) (min z12 z22)))))))

(defvar cuboid-memo (ht-create))

(defun cube-count (pairs &optional debug)
  (or (when-let ((pmemo (ht-get cuboid-memo pairs)))
        (debugmsg "Pairs %S already in memo" pairs)
        pmemo)
      (let ((count
             (if (not pairs)
                 0
               (let ((rest (cdr pairs)))
                 (seq-let (command1 cuboid1) (car pairs)
                   (let* ((pairsAB nil)
                          (countA (if (equal command1 "on")
                                      (cuboid-size cuboid1)
                                    0))
                          (countB (cube-count rest debug))
                          (countAB (cube-count
                                    (cl-loop
                                     for (_command2 cuboid2) in rest
                                     append
                                     (when-let ((cuboid12 (cuboid-intersect
                                                           cuboid1 cuboid2)))
                                       (list (list command1 cuboid12)))))))
                     (debugmsg "Cuboid %S has %d ON cubes" cuboid1 countA)
                     (- (+ countA countB) countAB)))))))
        (ht-set cuboid-memo pairs count)
        (debugmsg "Cuboids %S have %d on cubes in total"
                  (seq-map #'cadr pairs)
                  count)
        count)))

(defun day22 (&optional file debug)
  (let* ((pair-data (day22-data file)))
    (setq cuboid-memo (ht-create))
    (cube-count pair-data debug)))


;;   -------
;;  |       |
;;  | 9x9   |
;;  |     __|
;;  |    |  |
;;  |____|__|

;; (setq foo1 '((1 9) (1 9) (1 1)))
;; (setq foo2 '((6 12) (-4 2) (1 1)))
;; (setq foo3 '((7 17) (1 11) (1 1)))


;;;; Notes

;; Trust recursion!
;; But use memoization!
