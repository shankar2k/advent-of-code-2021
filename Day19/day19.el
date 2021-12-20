;;;; Header

(setq lexical-binding t
      max-lisp-eval-depth 8000
      max-specpdl-size 20000)

(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))

;;;; Task Functions

(defun get-orient (str)
  (cl-loop for ch across str
           with orient = '(1 2 3)
           when (eq ch ?I)
           return orient
           do (seq-let (x y z) orient
                (setq orient (cl-case ch
                               (?X (list x (- z) y))
                               (?Y (list (- z) y x))
                               (?Z (list (- y) x z)))))
           finally return orient))

(defvar all-orients
  (cl-loop for str in '("I" "X" "Y" "Z" "XX" "XY" "XZ" "YX" "YY" "ZY" "ZZ"
                        "XXX" "XXY" "XXZ" "XYX" "XYY" "XZZ" "YXX" "YYY" "ZZZ"
                        "XXXY" "XXYX" "XYXX" "XYYY")
           collect (get-orient str)))

(defun process-scanner-data (&optional file)
  (let* ((raw-data (aoc-input
                    file
                    '(blank-line (newline "--- scanner %*d ---"
                                          "%d,%d,%d")))))
    (cl-loop for sreport in raw-data
             collect (mapcar #'vconcat sreport))))

(defun ht-print (table)
  (apply #'concat (ht-map (lambda (k v) (format "%S --> %S\n" k v)) table)))

(defun ht-max (table)
  (cl-loop for key hash-keys of table using (hash-values val)
           with maxkey = nil with maxval = nil
           when (or (not maxkey) (> val maxval))
           do (setq maxkey key
                    maxval val)
           finally return (list maxkey maxval)))

(defun orient-coord (beacon shift)
  (if (< shift 0)
      (- (aref beacon (1- (- shift))))
    (aref beacon (1- shift))))

(defun orient-scanner (scanner orient)
  (cl-loop for beacon in scanner
           collect (vconcat (mapcar (lambda (shift)
                                      (orient-coord beacon shift))
                                    orient))))

(defun project-scanner (scanner orient shift)
  (cl-loop for beacon in (orient-scanner scanner orient)
           collect (vconcat (seq-mapn #'+ beacon shift))))

(defun compare-scanners (s1 s2)
  (let ((best-orient nil))
    (dolist (orient all-orients best-orient)
      (seq-let (shift scount)
          (let ((shift-counts (ht-create)))
            (dolist (beacon1 s1 (ht-max shift-counts))
              (dolist (beacon2 (orient-scanner s2 orient))
                (ht-inc shift-counts (seq-mapn #'- beacon1 beacon2)))))
        (when (or (not best-orient) (> scount (nth 2 best-orient)))
          (setq best-orient (list orient shift scount)))))))

(defun get-paths (node graph)
  (let* ((node-count (length graph))
         (visited (make-vector node-count nil))
         (node-queue (list (list node))))
    (while node-queue
      (seq-let (currnode currpath) (pop node-queue)
        (setf (aref visited currnode) (cons currnode currpath))
        (dotimes (n node-count)
          (when (and (not (aref visited n))
                     (or (aref2d graph n currnode)
                         (aref2d graph currnode n)))
            (nconc node-queue (list (list n (aref visited currnode))))))))
    visited))

(defun project-scanner-path (scanner node path sgraph &optional debug)
  (let ((total-shift [0 0 0]))
    (if (= (car path) node)
        (list scanner total-shift)
    (cl-loop for (current next) on path
             do (seq-let (orient shift _count) (or (aref2d sgraph current next)
                                                   (aref2d sgraph next current))
                  (debugmsg
                   "Applying orient %S and shift %S to map scanner %d to %d"
                   orient shift current next)
                  (setq total-shift (car (project-scanner (list total-shift)
                                                          orient shift)))
                  (debugmsg " --> total shift = %S" total-shift)
                  (setq scanner (project-scanner scanner orient shift)))
             when (= next node)
             return (list scanner total-shift)))))

(defun project-all-scanners (sdata scanner-graph paths start-node
                                   &optional debug)
  (cl-loop for scanner in sdata
           for path across paths
           for node from 0 below (length paths)
           with all-beacons = nil with all-shifts = nil
           do (seq-let (beacons total-shift)
                  (project-scanner-path scanner start-node path
                                        scanner-graph debug)
                (setq all-beacons (append all-beacons beacons))
                (setq all-shifts (append all-shifts (list total-shift))))
           finally return (list all-beacons all-shifts)))

(defun make-scanner-graph (sdata)
  (let* ((scanner-count (length sdata))
         (scanner-graph (make-vector2d scanner-count scanner-count nil)))
    (cl-loop for scanner1 in sdata
             for y from 0 below scanner-count
             do (cl-loop for scanner2 in sdata
                         for x from 0 below scanner-count
                         when (/= x y)
                         do (seq-let (orient shift count)
                                (compare-scanners scanner1 scanner2)
                              (debugmsg "Orienting scanners %d and %d" x y)
                              (when (>= count 12)
                                (setf (aref2d scanner-graph x y)
                                      (list orient shift count)))))
             finally return scanner-graph)))

(defun sort-beacon (b1 b2)
  (or (< (aref b1 0) (aref b2 0))
      (and (= (aref b1 0) (aref b2 0))
           (< (aref b1 1) (aref b2 1)))
      (and (= (aref b1 0) (aref b2 0))
           (= (aref b1 1) (aref b2 1))
           (< (aref b1 2) (aref b2 2)))))

(defun print-beacons (beacons)
  (string-join (cl-loop for beacon in beacons
                        collect (format "[%d,%d,%d]"
                                        (aref beacon 0)
                                        (aref beacon 1)
                                        (aref beacon 2))) "\n"))

(defun manhattan (v1 v2)
  (seq-sum (seq-mapn (lambda (x y) (abs (- x y))) v1 v2)))

(defun day19 (&optional file part2 start-node debug)
  (let* ((sdata (process-scanner-data file))
         (start-node (or start-node 0))
         (scanner-graph (make-scanner-graph sdata))
         (paths (get-paths start-node scanner-graph))
         (all-beacon-data (project-all-scanners sdata scanner-graph
                                                paths start-node debug))
         (beacons (sort (seq-uniq (car all-beacon-data)) #'sort-beacon)))
    (if (not part2)
        (length beacons)
      (let ((dists
             (cl-loop for shift-list on (cadr all-beacon-data)
                      when (cdr shift-list)
                      append (cl-loop for shift2 in (cdr shift-list)
                                      collect (manhattan (car shift-list)
                                                         shift2)))))
        (cons dists (seq-max dists))))))


;;;; Notes

;;;; Old Task Functions

(defun compare-scanners-old (s1 s2)
  (cl-loop for orient in all-orients with best-orient = nil
           do (seq-let (shift scount)
                  (let ((shift-counts (ht-create)))
                    (dolist (beacon1 s1 (ht-max shift-counts))
                      (dolist (beacon2 (orient-scanner s2 orient))
                        (ht-inc shift-counts (seq-mapn #'- beacon1 beacon2)))))
                (when (or (not best-orient) (> scount (nth 2 best-orient)))
                  (setq best-orient (list orient shift scount))))
           finally return best-orient))

(defun get-paths-old (node graph)
  (let* ((node-count (length graph))
         (visited (make-vector node-count nil))
         (node-queue (list (list node))))
    (while node-queue
      (seq-let (currnode currpath) (pop node-queue)
        (setf (aref visited currnode) (cons currnode currpath))
        (cl-loop for n from 0 below node-count
                 when (and (not (aref visited n))
                           (or (aref2d graph n currnode)
                               (aref2d graph currnode n)))
                 do (setq node-queue
                          (append node-queue
                                  (list (list n (aref visited currnode))))))))
    visited))

(defun make-scanner-graph-old (sdata)
  (let* ((scanner-count (length sdata))
         (scanner-graph (make-vector2d scanner-count scanner-count nil)))
    (cl-loop for scanner1 in sdata
             for y from 0 below scanner-count
             do (cl-loop for scanner2 in sdata
                         for x from 0 below scanner-count
                         when (/= x y)
                         do (seq-let (orient shift count)
                                (compare-scanners scanner1 scanner2)
                              (debugmsg "Orienting scanners %d and %d" x y)
                              (when (>= count 12)
                                (setf (aref2d scanner-graph x y)
                                      (list orient shift count))))))
    scanner-graph))
