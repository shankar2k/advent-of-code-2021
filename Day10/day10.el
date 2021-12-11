;;;; Header

(setq filename "input.txt")

(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))

;;;; Task Functions

(defun matching-char (ch)
  (let ((chars  "([{<)]}>")
        (mchars ")]}>([{<"))
    (elt mchars (seq-position chars ch))))

(defun get-illegal-char-or-completion (line &optional get-completion)
  (cl-loop for ch across line with stack = nil
           if (member ch (list ?\( ?\[ ?{ ?< ))
           do (push (matching-char ch) stack)
           else unless (eq (pop stack) ch)
           return (unless get-completion ch)
           finally return (when get-completion (seq-into stack 'string))))


(defun score-char (ch)
  (cl-case ch
    (?\)  3)
    (?\]  57)
    (?}  1197)
    (?>  25137)
    (nil 0)))


(defun day10-part1 (&optional file debug)
  (let* ((data (aoc-input file))
         (chars (delq nil (mapcar #'get-illegal-char-or-completion data)))
         (result (seq-sum (mapcar #'score-char chars))))
    (cons (seq-into chars 'string) result)))



(defun score-incomplete-line (line)
  (cl-loop for ch across line with total = 0
           do (setq total (+ (* 5 total)
                             (1+ (seq-position ")]}>" ch))))
           finally return total))


(defun day10-part2 (&optional file debug)
  (let* ((data (aoc-input file))
         (lines (mapcar (lambda (line) (get-illegal-char-or-completion line t))
                        data))
         (totals (mapcar #'score-incomplete-line (delq nil lines)))
         (stotals (sort totals #'>)))
    (cons stotals (nth (/ (length stotals) 2) stotals))))


;; (day10-part1)
;;  ===> ("]))>]]]]]>]]>}]]))]}>}])))>}))}])}]}>])])>>]>))>>}}" . 288291)

;; (day10-part2)
;;  ===> ((23914974118 23891372932 22771476108 21303822857 21171717987 19963396657 16697008992 16478490591 16441913939 16229139322 8230070531 5972167312 5551442459 4768163361 4457960423 4234902481 3638145319 3579394737 3239420533 3090879738 2860880457 2800510593 1819031193 1187005161 874436213 820045242 810289839 673008908 669254789 653261468 478337298 439177719 436511748 436509359 340727947 321778989 120962314 96677982 83888969 34880546 15601438 13493446 13467789 5757484 5210406 3479997 1404573 1074348 676121 212368 55372) . 820045242)

;;;; Notes


;;;; Old Task Functions

(defun get-illegal-chars-old (data)
  (cl-loop for line in data
           collect (cl-loop for ch across line with stack = nil
                            if (member ch (list ?\( ?\[ ?{ ?< ))
                            do (push ch stack)
                            else
                            unless (eq (pop stack)
                                       (cl-case ch
                                         (?\) ?\( )
                                         (?\] ?\[ )
                                         (?}  ?{ )
                                         (?>  ?< )))
                            return ch)))

(defun day10-part1-old (&optional file debug)
  (let* ((data (aoc-input file))
         (chars (remove nil (get-illegal-chars-old data)))
         (result (cl-loop for ch in chars
                          sum  (cl-case ch
                                 (?\)  3)
                                 (?\]  57)
                                 (?}  1197)
                                 (?>  25137)
                                 (nil 0)))))
         (cons chars result)))

(defun get-incomplete-lines-old (data)
  (remove nil (cl-loop for line in data
                       collect (cl-loop for ch across line with stack = nil
                                        if (member ch (list ?\( ?\[ ?{ ?< ))
                                        do (push (cl-case ch
                                                   (?\( ?\) )
                                                   (?\[ ?\] )
                                                   (?{  ?}  )
                                                   (?<  ?>  )) stack)
                                        else
                                        unless (eq (pop stack) ch)
                                        return nil
                                        finally return (seq-into stack
                                                                 'string)))))

(defun score-incomplete-line-old (line)
  (cl-loop for ch across line with total = 0
           do (setq total (+ (* 5 total)
                             (cl-case ch
                               (?\) 1)
                               (?\] 2)
                               (?} 3)
                               (?> 4))))
           finally return total))

(defun day10-part2-old (&optional file debug)
  (let* ((data (aoc-input file))
         (lines (get-incomplete-lines-old data))
         (totals (mapcar #'score-incomplete-line-old lines))
         (stotals (sort totals #'>)))
    (cons stotals (nth (/ (length stotals) 2) stotals))))
