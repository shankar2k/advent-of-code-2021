;;;; Header

(setq lexical-binding t
      max-lisp-eval-depth 2000
      max-specpdl-size 10000)


(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))
;; Download from heap.el http://www.dr-qubit.org/emacs.php
;;(require 'heap (expand-file-name "../heap.el"))

;;;; Task Functions

(defun get-hex-table (&optional hex-file)
  (cl-loop for (hex bin) in (aoc-input (or hex-file "hex2bin.txt")
                                       '(newline "%s = %s"))
           with hex-table = (ht-create)
           do (ht-set hex-table hex bin)
           finally return hex-table))

(defun hex2bin (hdata &optional hex-table)
  (cl-loop for ch across hdata
           concat (ht-get (or hex-table (get-hex-table))
                          (char-to-string ch))))

(defvar packet-index 0
  "Global string index for processing packet")

(defun get-int (bdata n &optional debug)
  (let* ((word (substring bdata packet-index (+ packet-index n)))
         (ival (str2num word 2)))
    (cl-incf packet-index n)
    (debugmsg "%d-bit int %s --> %d\nrest: %s\n"
              n word ival (s-truncate 50 (substring bdata packet-index)))
    ival))

(defun get-str (bdata n)
  (let* ((word (substring bdata packet-index (+ packet-index n))))
    (cl-incf packet-index n)
    word))

(defun int= (x y)
  (if (= x y) 1 0))

(defun int< (x y)
  (if (< x y) 1 0))

(defun int> (x y)
  (if (> x y) 1 0))

(defun get-operator (type-id)
  (aref [+ * min max value int> int< int=] type-id))

(defun get-packet (bdata &optional debug reset-index)
  (when reset-index
    (setq packet-index 0))
  (debugmsg "Initial: %s" (s-truncate 50 (substring bdata packet-index)))
  (let* ((version (get-int bdata 3 debug))
         (type-id (get-int bdata 3 debug)))
    (seq-let (pversions pdata)
        (cl-case type-id
          (4 (get-value-packet bdata debug))
          (t (get-operator-packet bdata debug)))
      (list (cons version pversions)
            (if (= type-id 4)
                pdata
              (cons (get-operator type-id) pdata))))))

(defun get-value-packet (bdata &optional debug)
  (let ((value-str "")
        (found-last-group nil))
    (while (not found-last-group)
      (let ((packet (get-str bdata 5)))
        (setq value-str (concat value-str (substring packet 1)))
        (when (eq (aref packet 0) ?0)
          (setq found-last-group t))))
    (let ((value  (str2num value-str 2)))
      (debugmsg "Value: %s --> %d\nrest: %s\n"
                value-str value (s-truncate 50 (substring bdata packet-index)))
      (list nil value))))


(defun get-operator-packet (bdata &optional debug)
  (setq start-index packet-index)
  (let* ((length-id (get-str bdata 1))
         (do-length (equal length-id "0"))
         (len-or-count (progn
                         (debugmsg (if do-length
                                       "0 --> length operator"
                                   "1 --> count operator"))
                         (get-int bdata (if do-length 15 11) debug)))
         (packet-end (+ packet-index len-or-count))
         (n 0)
         (all-versions nil)
         (all-data nil))
      (while (if do-length
                 (< packet-index packet-end)
               (< n len-or-count))
        (cl-incf n)
        (seq-let (sversion sdata) (get-packet bdata debug)
          (setq all-versions (append all-versions sversion))
          (push sdata all-data)))
      (debugmsg "Oper: %s -->\nversions: %S\ncode:\n%s\nrest: %s\n"
                (substring bdata start-index packet-index)
                all-versions
                (pp all-data)
                (s-truncate 50 (substring bdata packet-index)))
      (list all-versions (nreverse all-data))))

(defun day16 (&optional file part2 debug)
  (let* ((hdata (aoc-input file 'line))
         (hex-table (get-hex-table))
         (bdata (hex2bin hdata hex-table)))
    (seq-let (versions operations) (get-packet bdata debug t)
      (if part2
          (progn
            (when (eq part2 'display)
              (message "%S" operations)
              (eval operations)))
      (seq-sum versions)))))

;;;; Notes

;; - Remember scope:
;;   - If variable not defined as argument,
;;   - then will look in the surrounding context
;;   - use lexically scope variables to keep track
;;   - global packet index leads to cleanest solution

;;;; Old Task Functions



(defun get-packet-old (bdata index &optional debug)
  (debugmsg "Initial: %s" (s-truncate 50 (substring bdata index)))
  (let* ((version (get-int bdata index 3 debug))
         (type-id (get-int bdata (+ index 3) 3 debug)))
    (cl-incf index 6)
    (let ((packet-data (cl-case type-id
                          (4 (get-value-packet-old bdata index debug))
                          (t (get-operator-packet-old bdata index debug)))))
      (setq index (car packet-data))
      (list version type-id index (cdr packet-data)))))


(defun get-value-packet-old (bdata index &optional debug)
  (let ((value-str "")
        (found-last-group nil))
    (while (not found-last-group)
      (let ((packet (substring bdata index (+ index 5))))
        (cl-incf index 5)
        (setq value-str (concat value-str (substring packet 1)))
        (when (eq (aref packet 0) ?0)
          (setq found-last-group t))))
    (debugmsg "Value: %s --> %d\nrest: %s\n"
              value-str
              (str2num value-str 2)
              (s-truncate 50 (substring bdata index)))
    (cons index (str2num value-str 2))))

(defun get-operator-packet-old (bdata oindex &optional debug)
  (let* ((length-id (aref bdata oindex)))
    (if (eq length-id ?0)
        (debugmsg "0 --> length operator")
      (debugmsg "1 --> count operator"))
    (setq start-oindex oindex)
    (cl-incf oindex)
    (let ((result
           (if (eq length-id ?0)
               (let* ((packet-length (get-int bdata oindex 15 debug))
                      (packet-start (cl-incf oindex 15))
                      (packet-end (+ packet-start packet-length)))
                 (cl-loop while (< oindex packet-end)
                          collect (let ((sub-packet
                                         (get-packet-old bdata oindex debug)))
                                    (setq oindex (nth 2 sub-packet))
                                    sub-packet) into packet-list
                                    finally return (cons oindex packet-list)))
             (let* ((packet-count (get-int bdata oindex 11 debug)))
               (cl-incf oindex 11)
               (cl-loop for n from 0 below packet-count
                        collect (let ((sub-packet
                                       (get-packet-old bdata oindex debug)))
                                  (setq oindex (nth 2 sub-packet))
                             sub-packet) into packet-list
                             finally return (cons oindex packet-list))))))
      (debugmsg "Oper: %s --> %S\nrest: %s\n"
                (substring bdata start-oindex oindex)
                result
                (s-truncate 50 (substring bdata oindex)))
      result)))


;; (hex2bin "8A004A801A8002F478")
;;  ===> "100010100000000001001010100000000001101010000000000000101111010001111000"

(defun sum-versions-old (packet)
  (seq-let (pversion _ _ pdata) packet
    (if (listp pdata)
        (+ pversion (seq-sum (mapcar #'sum-versions-old pdata)))
      pversion)))



(defun bool2int-old (bool)
  (if bool 1 0))

(defun eval-packet-old (packet)
  (seq-let (pversion ptype _pindex pdata) packet
    (if (= ptype 4)
        pdata
      (let ((values (mapcar #'eval-packet-old pdata)))
        (if (member ptype '(0 1 2 3))
            (cl-case ptype
              (0 (seq-sum values))
              (1 (seq-product values))
              (2 (seq-min values))
              (3 (seq-max values)))
          (seq-let (a b) values
            (bool2int-old (cl-case ptype
                            (5 (> a b))
                            (6 (< a b))
                            (7 (= a b))))))))))

(defun day16-old (&optional file part2 debug)
  (let* ((hdata (aoc-input file 'line))
         (hex-table (get-hex-table))
         (bdata (hex2bin hdata hex-table))
         (packet (get-packet-old bdata 0 debug)))
    (if part2
        (eval-packet-old packet)
      (sum-versions-old packet))))
