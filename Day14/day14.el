;;;; Header

(setq filename "input.txt"
      max-lisp-eval-depth 2000
      max-specpdl-size 10000)


(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))

;;;; Task Functions

(defun get-char-tables (template)
  (let ((char-counts (ht-create))
        (word-counts (ht-create))
        (len (length template)))
    (cl-loop for n below len
             do (ht-inc char-counts (substring template n (1+ n)))
             when (< n (1- len))
             do (ht-inc word-counts (substring template n (+ n 2))))
    (list char-counts word-counts)))

(defun print-char-tables (char-counts word-counts)
  (format
   "%s, %s"
   (s-join " " (ht-map (lambda (k v) (format "%s:%d" k v)) char-counts))
   (s-join " " (ht-map (lambda (k v) (format "%s:%d" k v)) word-counts))))

(defun update-counts (rule old-word-counts char-counts word-counts)
  (when-let* ((old (car rule))
              (old-count (ht-get old-word-counts old 0)))
    (let* ((newch (cadr rule))
           (new1 (concat (substring old 0 1) newch))
           (new2 (concat newch (substring old 1 2))))
      (ht-inc word-counts old (- old-count))
      (when (zerop (ht-get word-counts old))
        (ht-remove word-counts old))
      (ht-inc word-counts new1 old-count)
      (ht-inc word-counts new2 old-count)
      (ht-inc char-counts newch old-count))))

(defun day14 (&optional file step-count debug)
  (seq-let (template rules)
      (aoc-input file '(blank-line "%s" (newline "%s -> %s")))
    (seq-let (chars words) (get-char-tables template)
      (debugmsg "Template:      %s" template)
      (debugmsg "Initial: %s" (print-char-tables chars words))
      (dotimes (step step-count)
        (cl-loop for rule in rules
                 with new-chars = (ht-copy chars)
                 with new-words = (ht-copy words)
                 do (update-counts rule words new-chars new-words)
                 finally do
                 (setq chars new-chars
                       words new-words)
                 (debugmsg "After step %2d: %s" (1+ step)
                           (print-char-tables chars words))))
      (let ((most (seq-max (ht-values chars)))
            (least (seq-min (ht-values chars))))
        (list most least (- most least))))))

;; (process-instructions "input_test.txt")
;;  ===> ("NNCB" ("CH" "B") ("HH" "N") ("CB" "H") ("NH" "C") ("HB" "C")
;;               ("HC" "B") ("HN" "C") ("NN" "C") ("BH" "H") ("NC" "B")
;;               ("NB" "B") ("BN" "B") ("BB" "N") ("BC" "B") ("CC" "N")
;;               ("CN" "C"))

;;;; Notes

;; - Make sure existing functions (like s-matched-positions-all) work the way you intended
;;   - Didn't find overlapping matches
;; - Use clue that size of problem grows exponentially to avoid computing in memory

;;;; Old Task Functions

(defun find-all-matches (word template)
  (cl-loop for n below (1- (length template))
           when (equal word (substring template n (+ n 2)))
           collect (1+ n)))

(defun process-instructions-old (&optional file)
  (let* ((data (aoc-input file '(blank-line "%s")))
         (template (s-chomp (car data)))
         (rules (cl-loop for line in (s-lines (cadr data))
                         collect (s-split " -> " line))))
    (cons template rules)))

(defun day14-part1 (&optional file step-count debug)
  (let* ((pdata (process-instructions-old file))
         (template (car pdata))
         (rules (cdr pdata)))
    (debugmsg "Template:      %s" template)
    (cl-loop for step below step-count
             do (let* ((inserts (cl-loop for rule in rules
                                         when (setq inds (find-all-matches
                                                          (car rule) template))
                                         append (sbr-pairlis inds (cadr rule))))
                       (sinserts (sort inserts
                                       (lambda (x y) (> (car x) (car y))))))
                  (debugmsg "Insertions %2d (%d): %S"
                            (1+ step) (length sinserts) sinserts)
                  (cl-loop for sinsert in sinserts
                           do (setf (substring template
                                               (car sinsert) (car sinsert))
                                    (cdr sinsert)))
                  (debugmsg "After step %2d (%d): %s"
                            (1+ step) (length template) template)))
    (cl-loop for ch across template
             with poly-table = (ht-create)
             do (ht-set poly-table ch (1+ (ht-get poly-table ch 0)))
             finally return
             (let* ((chars  (seq-into (ht-keys poly-table) 'string))
                    (counts (ht-values poly-table))
                    (most (apply #'max counts))
                    (least (apply #'min counts)))
               (list chars counts most least (- most least))))))

(defun get-char-tables-old (template)
  (let ((char-counts (ht-create))
        (word-counts (ht-create)))
    (cl-loop for ch across template
             do (ht-set char-counts (char-to-string ch)
                        (1+ (ht-get char-counts (char-to-string ch) 0))))
    (cl-loop for n below (1- (length template))
             do (ht-set word-counts (substring template n (+ n 2))
                        (1+ (ht-get word-counts
                                    (substring template n (+ n 2)) 0))))
    (list char-counts word-counts)))


(defun update-counts-old (rule old-word-counts char-counts word-counts)
  (let* ((old (car rule))
         (old-count (ht-get old-word-counts old 0)))
    (when (> old-count 0)
      (let* ((newch (cadr rule))
             (new1 (concat (substring old 0 1) newch))
             (new2 (concat newch (substring old 1 2)))
             (new-old-count (- (ht-get word-counts old) old-count)))
        ;;(ht-remove word-counts old)
        (if (> new-old-count 0)
            (ht-set word-counts old new-old-count)
          (ht-remove word-counts old))
        (ht-set word-counts new1 (+ old-count (ht-get word-counts new1 0)))
        (ht-set word-counts new2 (+ old-count (ht-get word-counts new2 0)))
        (ht-set char-counts newch (+ old-count
                                     (ht-get char-counts newch 0)))))))

(defun day14-part2-old (&optional file step-count debug)
  (let* ((pdata (process-instructions file))
         (template (car pdata))
         (rules (cdr pdata)))
    (seq-let (char-counts word-counts) (get-char-tables-old template)
      (debugmsg "Template:      %s" template)
      (debugmsg "Initial: %s" (print-char-tables-old char-counts word-counts))
      (cl-loop for step from 1 to step-count
               do (cl-loop for rule in rules
                           with new-char-counts = (ht-copy char-counts)
                           with new-word-counts = (ht-copy word-counts)
                           do (update-counts-old
                               rule word-counts new-char-counts new-word-counts)
                           finally do
                           (setq char-counts new-char-counts
                                 word-counts new-word-counts)
                           (debugmsg
                            "After step %2d: %s" step
                            (print-char-tables char-counts word-counts))))
      (let ((most (apply #'max (ht-values char-counts)))
            (least (apply #'min (ht-values char-counts))))
        (list most least (- most least))))))
