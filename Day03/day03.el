;;;; Header

(setq filename "input.txt")

(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))

;;;; Task Functions

                    
(defun day03-part1 (&optional file)
  (let* ((data (aoc-input file 'string))
         (len (length data))
         (counts (seq-into (apply #'seq-add data) 'list))
         (offset (* ?0 len))
         (result (cl-loop for icount in counts
                          if (> (- icount offset) (/ len 2))
                          concat "0" into gammastr
                          and concat "1" into epsstr
                          else
                          concat "1" into gammastr
                          and concat "0" into epsstr
                          finally return (list gammastr epsstr))))
    (message "counts=%S" counts)
    (seq-let (gamma epsilon) (str2num result 2)
      (list gamma epsilon (+ gamma epsilon) (* gamma epsilon)))))

;; (day03-part1) 
;;  ===> (3676 419 4095 1540244)                  

(defun get-index-rating (data index most-common-p)
  (if (= (length data) 1)
      data
    (let ((zcount (cl-loop for line in data
                           count (eq (aref line index) ?0))))
      (setq want-zeroes-p (eq (> zcount (/ (length data) 2)) most-common-p))
      (message "For index %d, zcount=%d/%d, want-zeroes=%S" index zcount (length data) want-zeroes-p)
      (cl-loop for line in data
               when (eq (eq (aref line index) ?0) want-zeroes-p)
               collect line))))

(defun day03-part2 (&optional file)
  (let* ((odata (aoc-input file))
         (sdata (copy-sequence odata)))
    (cl-loop for ind below (length (car odata))
             do (setq odata (get-index-rating odata ind t)
                      sdata (get-index-rating sdata ind nil))
             (message "len odata=%d, len sdata=%d" (length odata) (length sdata))
             finally return (let* ((ostr (car odata))
                                   (sstr (car sdata))
                                   (orating (string-to-number ostr 2))
                                   (srating (string-to-number sstr 2)))
                              (list ostr orating sstr srating (* orating srating))))))

;; (day03-part2) 
;;  ===> ("010010001001" 1161 "111000100101" 3621 4203981)

                    
;;;; Old Versions

(defun day03-part1-old (&optional file)
  (let* ((data (aoc-input file))
         (counts (make-vector (length (car data)) 0)))
    (cl-loop for line in data
             do (cl-loop for ind across line
                         for icount across-ref counts
                         when (eq ind ?0)
                         do (cl-incf icount)))
    (message "counts = %S" counts)
    (cl-loop for icount across counts with gamma = 0 with epsilon = 0
             if (> icount (/ (length data) 2))
             do (setq gamma (1+ (* 2 gamma))
                      epsilon (* 2 epsilon))
             else do (setq gamma (* 2 gamma)
                           epsilon (1+ (* 2 epsilon)))
             finally return (list gamma epsilon (+ gamma epsilon) (* gamma epsilon)))))

