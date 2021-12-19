;;;; Header

(setq lexical-binding t
      max-lisp-eval-depth 8000
      max-specpdl-size 20000)

(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))

;;;; Task Functions

(defun snail-mag (num)
  (cond ((numberp num) num)
        ((stringp num) (snail-mag (snail-str2num num)))
        (t (seq-let (left right) num
               (+ (* 3 (snail-mag left))
                  (* 2 (snail-mag right)))))))

(defun snail-mag-str (str)
  (snail-mag (car (read-from-string str))))



(defun snail-str2num (s)
  (s-replace-all '(("[" . "(")
                   ("]" .")")
                   ("," . " ")) s))

(defvar snail-start 0)

(defvar snail-index 0)

(defvar snail-depth 0)

(defvar snail-string "")

(defvar snail-regex "\\((+\\|[0-9]+\\| \\|)+\\)")

(defun snail-get-token ()
  (setq snail-start (string-match snail-regex snail-string snail-index))
  (setq snail-index (match-end 0))
  (substring snail-string snail-start snail-index))

(defun snail-add-str (num1 num2 &optional debug)
  (setq sum12 (concat "(" num1 " " num2 ")"))
  (debugmsg "after addition: %S" sum12)
  (snail-reduce-str sum12 debug))


(defun snail-split-str (val &optional debug)
  (setq tmp snail-string)
  (setf (substring snail-string snail-start snail-index)
        (format "(%d %d)" (/ val 2) (/ (1+ val) 2)))
  (debugmsg "After split %d: %s" val snail-string))

(defun snail-reset (&optional reset-str)
  (setq snail-index 0)
  (setq snail-depth 0)
  (setq snail-start 0)
  (when reset-str
    (setq snail-string reset-str)))

(defun snail-get-left ()
  (with-temp-buffer
    (insert (substring snail-string 0 (- snail-start 1)))
    (when (re-search-backward "[0-9]+" 0 t)
      (setq left-start (1- (match-beginning 0)))
      (setq left-end (1- (match-end 0)))
      (setq match-str (match-string 0))
      ;; SBR: Crazy hack to deal with problem with re-search-backward
      (setq left-num (thing-at-point 'number))
      (when (/= left-num (str2num match-str))
        ;; if re-search-backward gets a wrong length integer, use
        ;; thing-at-point to get the right one, and modify the start position
        ;; accordingly
        (cl-decf left-start (- (length (number-to-string left-num))
                               (length match-str))))
      (list left-num left-start left-end))))

;; "((((4 0) (5 4)) ((7 7) (6 0))) (((6 6) (5 6)) ((6 0) (7 7))))"

;; From Reddit u/ipav:
;; explode: [[[[12,12],[6,14]],[[15,0],[17,[8,1]]]],[2,9]]
;; explode: [[[[12,12],[6,14]],[[15,0],[25,0]]],[3,9]]

;; My output
;; After explode (15 9): ((((12 12) (6 14)) ((15 0) (17 (8 1)))) (2 9))
;;   explode right=2 by 1 at [41-42]
;;   explode left=7 by 8 at (29-30)
;; After explode (8 1): ((((12 12) (6 14)) ((15 0) (115 0))) (3 9))


(defun snail-get-right (rparen)
  (setq right-start (string-match "[0-9]+" snail-string rparen))
  (when right-start
      (setq right-end (match-end 0))
      (setq right-str (substring snail-string right-start right-end))
      (list (str2num right-str) right-start right-end)))

(defun snail-explode-str (&optional debug)
  (let* ((rparen (string-match ")" snail-string snail-index))
         (explode-str (substring snail-string (1- snail-start) (1+ rparen)))
         (explode-int (s-scan "(%d %d)" explode-str))
         (left (snail-get-left))
         (right (snail-get-right rparen)))
    (setq tmp snail-string)
    (seq-let (rval rstart rend) (snail-get-right rparen)
      (debugmsg "  explode right=%S by %d at [%S-%S]"
                rval (cadr explode-int) rstart rend)
      (when rval
        (setf (substring snail-string rstart rend)
            (format "%d" (+ rval (cadr explode-int))))))
    (setf (substring snail-string (1- snail-start) (1+ rparen))
          (format "%d" 0))
    (seq-let (lval lstart lend) (snail-get-left)
      (debugmsg "  explode left=%S by %d at (%S-%S)"
                lval (car explode-int) lstart lend)
      (when lval
        (setf (substring snail-string lstart lend)
              (format "%d" (+ lval (car explode-int))))))
    (debugmsg "After explode %s: %s" explode-str snail-string)))

(defun snail-reduce-str (&optional reset-str debug)
  (snail-reset reset-str)
  (setq was-reduced t)
  (while was-reduced
    (setq was-reduced nil)
    (catch 'reduced
      (dotimes (iter 2)
        (snail-reset)
        (cl-loop while (< snail-index (length snail-string))
                 do
                 (setq mstr (snail-get-token))
                 (cond ((string-match-p "(+" mstr)
                        (cl-incf snail-depth (length mstr)))
                       ((string-match-p "[0-9]+" mstr)
                        (when (and (= iter 0) (> snail-depth 4))
                          (snail-explode-str debug)
                          (setq was-reduced t)
                          (throw 'reduced nil))
                        (when (and (= iter 1) (>= (str2num mstr) 10))
                          (snail-split-str (str2num mstr) debug)
                          (setq was-reduced t)
                          (throw 'reduced nil)))
                       ((string-match-p ")+" mstr)
                        (cl-decf snail-depth (length mstr)))))))))

(defun mag-add (snum1 snum2 &optional debug)
  (snail-add-str snum1 snum2 debug)
  (snail-mag-str snail-string))

(defun day18-part1 (&optional file sum-count debug)
  (let* ((data (aoc-input file 'string))
         (snail-numbers (cl-loop for line in data
                                 collect (snail-str2num line)))
         (total (car snail-numbers)))
    (cl-loop for snum in (cdr snail-numbers)
             for n from 1 to (or sum-count (length (cdr snail-numbers)))
             do (snail-add-str total snum debug)
             (setq total snail-string)
             finally return (cons (snail-mag-str snail-string)
                                  snail-string))))

(defun day18-part2 (&optional file debug)
  (let* ((data (aoc-input file 'string))
         (snail-numbers (cl-loop for line in data
                                 collect (snail-str2num line))))
    (cl-loop for num1 on snail-numbers
             when (cdr num1)
             append
             (cl-loop for num2 in (cdr num1)
                      do (message "Mag of %s + %s (and vice versa)"
                                  (car num1) num2)
                      append (list (mag-add (car num1) num2 debug)
                                   (mag-add num2 (car num1) debug)))
             into mag-list
             finally return (cons mag-list (seq-max mag-list)))))

;;;; Notes

;; - consp is not same as listp!

;;;; Old Task Functions

(defun snail-add (num1 num2 &optional debug)
  (setcar num1 (copy-sequence num1))
  (setcdr num1 (list num2))
  (debugmsg "after addition: %S" num1)
  (snail-reduce num1 num1 nil nil 0 debug))

(defun snail-explode (whole pair left right &optional debug)
  ;; snail-reduce
  (seq-let (pleft pright) (car pair)
    (when left
      (setcar left (+ (car left) pleft)))
    (when right
      (setcar right (+ (car right) pright))))
  (setcar pair 0)
  (debugmsg "after explode: %S" whole)
  (snail-reduce whole whole nil nil 0 debug))


(defun snail-reduce (whole current left right depth &optional debug)
  (cond ((or (null current) (numberp current)) whole)
        ((and (>= depth 3) (is-snail-pair (car current)))
         (debugmsg "!! exploding %S rawleft=%s left=%S right=%S!!"
                   current left (snail-left-regular left current)
                   (snail-right-regular (cdr current) right))
         (snail-explode whole current (snail-left-regular left current)
                        (snail-right-regular (cdr current) right) debug))
        ((and (is-snail-regular current) (>= (car current) 10))
         (debugmsg "!! splitting %S !!" (car current))
         (snail-split whole current debug))
        ((consp current)
         (when (consp (car current))
           (snail-reduce whole (car current) left
         (when (consp (cdr current))
           (snail-reduce whole (cdr current)
                         (snail-left-regular (car current) (cdr current))
                         right depth debug)))
        (t whole))
  whole)

;; (setq foo (snail-str2num "[[[[[9,8],1],2],3],4]")
;; (setq foo-pair  (car (car (car foo))))
;; (setq foo-left  nil)
;; (setq foo-right (cdr (car (car (car foo)))))

;; (setq baz (snail-str2num "[7,[6,[5,[4,[3,2]]]]]"))
;; (setq baz-pair (cdr (cadr (cadr (cadr baz)))))
;; (setq baz-left (cadr (cadr (cadr baz))))
;; (setq baz-right (cdr (cdr (cadr (cadr (cadr baz))))))
(defun is-snail-regular (snum)
  (and snum (numberp (car snum))))

(defun is-snail-pair (snum)
  (seq-let (left right) snum
    (and (numberp left) (numberp right))))

(defun is-snail-single (snum)
  (and (consp snum)
       (numberp (car snum))
       (null (cdr snum))))

(defun snail-left-regular (left-snum current)
  (cond ((not (consp left-snum)) nil)
        ((is-snail-single left-snum)
         left-snum)
        ((is-snail-pair left-snum)
         (if (eq (cdr left-snum) current)
             left-snum
           (cdr left-snum)))
        ((eq (cdr left-snum) current)
         (if (is-snail-regular left-snum)
             left-snum
           (snail-left-regular (car left-snum) current)))
        ((not (consp (cdr left-snum)))
         (snail-left-regular (car left-snum) current))
        (t (snail-left-regular (cdr left-snum) current))))

(defun snail-right-regular (right-snum default)
  (cond ((null right-snum) default)
        ((is-snail-regular right-snum) right-snum)
        (t (snail-right-regular (car right-snum) default))))


(defun snail-split (whole num &optional debug)
  (setcar num (list (/ (car num) 2) (/ (1+ (car num)) 2)))
  (debugmsg "after split: %s" whole)
  (snail-reduce whole whole nil nil 0 debug))


;; Aborted approach to manipulate a list of depths and list of regular numbers
;; I think this could lead to an elegant solution

(defun get-reg-numbers (str)
  (cl-loop for mlist in (s-match-strings-all "[0-9]+" str)
           collect (str2num (car mlist))))

(defun get-depths (ptree depth)
  (if (numberp ptree)
      (list depth)
    (if (consp ptree)
        (append (get-depths (car ptree) (1+ depth))
                (get-depths (cdr ptree) depth)))))

;; (get-reg-numbers "((((6 6) (6 6)) ((6 0) (6 7))) (((7 7) (8 9)) (8 (8 1))))")
;;  ===> (6 6 6 6 6 0 6 7 7 7 8 9 8 8 1)

;; (get-depths (car (read-from-string "((((6 6) (6 6)) ((6 0) (6 7))) (((7 7) (8 9)) (8 (8 1))))")) 0)
;;  ===> (4 4 4 4 4 4 4 4 4 4 4 4 3 4 4)

;; (get-reg-numbers foo3)
;;  ===> (6 6 6 6 6 0 6 7 7 7 8 9 8 8 1 2 9)

;; (get-depths (car (read-from-string foo3)) 0)
;;  ===> (5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 2 2)

