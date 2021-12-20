;;; aoc-helpers.el --- Helper functions for Advent of Code 2021  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Shankar Rao

;; Author: Shankar Rao <shankar.rao@gmail.com>
;; URL: https://github.com/~shankar2k/advent-of-code-2021

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

;;;; Requirements

(require 's)
(require 'f)
(require 'ht)

;;;; Input / Loading  Functions

(defvar aoc-input-file "input.txt"
  "Default input file for Advent of Code puzzles.")

(defvar aoc-test-file "input_test.txt"
  "Default test input file for Advent of Code puzzles.")

(defun aoc-input (&optional file format-spec type)
  (let* ((data (f-read (cond ((stringp file) file)
                             ((and file (symbolp file))
                              (if (eq file 'test)
                                  aoc-test-file
                                (error "Unknown file parameter %S" file)))
                             (t aoc-input-file))))
         (format-spec  (pcase (or format-spec 'string)
                         ((or (pred stringp) (pred listp))
                          format-spec)
                         ('string '(newline "%s"))
                         ('int    '(newline "%d"))
                         ('line   "%s"))))
    (s-scan format-spec data type)))

;; SBR: Older, more limited version that doesn't use s-scan
(defun aoc-input-old (&optional file type delim)
  (let* ((file (or file filename))
         (type (or type 'string))
         (delim (or delim "\n"))
         (sdata (with-temp-buffer
                  (insert-file-contents file)
                  (if (eq type 'line)
                      (s-chomp (buffer-string))
                    (split-string (s-chomp (buffer-string)) delim)))))
    (if (eq type 'int)
        (str2num sdata)
      sdata)))

;;;; Display Functions
(defun sbr-print (debug &rest args)
  (if debug
      (apply #'message args)
    (insert (apply #'format args))))

(defmacro debugmsg (format-string &rest args)
  (list 'when 'debug (append (list 'message format-string) args)))

;;;; String Functions

(defun str2num (x &optional base)
  (if (stringp x)
      (string-to-number x base)
    (cl-loop for str in x
             collect (string-to-number str base))))

(defun bstr2int (bstr)
  (cl-loop for c across bstr
           sum (if (eq c ?1) (1+ total) total) into total
           finally return total))


(defun s-scan--format-regexp (format-string)
  (let ((ctrl-regexp (rx "%" (optional "*") (any "cds"))))
    (cl-loop for ind = 0 then (match-end 0)
             while (string-match ctrl-regexp format-string ind)
             for ctrl = (match-string 0 format-string)
             concat (substring format-string ind
                               (match-beginning 0)) into format-regexp
             concat (s-scan--control-regexp ctrl) into format-regexp
             unless (s-scan--is-ignored ctrl)
             collect ctrl into ctrl-strings
             finally return (list format-regexp ctrl-strings))))

(defun s-scan--is-ignored (control)
  (eq (aref control 1) ?*))

(defun s-scan--control-regexp (control)
  (let ((regex (pcase (aref control (1- (length control)))
                 (?s (rx (1+ anything)))
                 (?c (rx anything))
                 (?d (rx (opt "-") (1+ digit))))))
    (if (s-scan--is-ignored control)
        regex
      (rx (group (regexp regex))))))


(defun s-scan--process-control (string control)
  (pcase control
    ("%s" string)
    ("%c" (string-to-char string))
    ("%d" (string-to-number string))))


(defun s-scan-string (format-string s &optional type)
  (let ((s (s-chomp s)))
    (seq-let (format-regexp controls) (s-scan--format-regexp format-string)
      (when (string-match format-regexp s)
        (if (= (length controls) 1)
            (s-scan--process-control (match-string 1 s) (car controls))
          (seq-into (cl-loop for ctrl element of controls using (index n)
                             collect (s-scan--process-control
                                      (match-string (1+ n) s) ctrl))
                    (or type 'list)))))))

(defun s-scan--delim (delim)
  (if (stringp delim)
      delim
    (cl-case delim
      (newline "\n")
      (endl "\n")
      (blank-line "^\n")
      (t (error "Unrecognized delimiter %S" delim)))))


(defun s-scan (format-spec s &optional type)
  "Extract parameters from string S subject to FORMAT-SPEC.

If FORMAT-SPEC is a string, then it is a format control string
similar to the one used by ``format''. FORMAT-SPEC should contain
one or more %-sequences specifying parameters to extract from S.
Currently, the following control codes are recognized:

%s means extract a string parameter.
%d means extract a signed number in decimal.
%c means extract a single character.

A %-sequence may contain an optional * flag, which indicates that
the parameter is to be read from S but not extracted.

If FORMAT-SPEC is a list, it should be of the form
(DELIM FMT-1 FMT-2 ...), where DELIM is a delimiter used to split S
into substrings, and FMT-1, FMT-2, FMT-3, ..., FMT-M are format specs used
to extract parameters from the substrings of S into a list.

DELIM can either be a regular expression similar to the one used
by ``split-string'' or one of the symbols 'newline, 'endl, which
split S on newline characters, or 'blank-line, which splits S on
blank lines.

The format spec FMT-I is applied the i-th substring split from S,
and just like FORMAT-SPEC, FMT-I can be either a string or a
list, enabling hierarchical extraction of parameters from S. If
there are less format specs FMT-I then there are substrings of S,
then the last format spec FMT-M will be applied to the remaining
substrings. If no FMT-I are provided, then the default format
\"%s\" is used.

Optional parameter TYPE converts the extracted parameter sequence
to a different type. TYPE can be one of the following symbols:
vector, string or list. The conversion is applied deeply to
sub-sequences, and thus for hierarchical parameter data, only the
type vector and list are supported."
  (if (stringp format-spec)
      (s-scan-string format-spec s type)
    (let ((delim (s-scan--delim (car format-spec)))
          (format-list (or (cdr format-spec) '("%s"))))
      (seq-into (cl-loop for token in (split-string s delim t "\n")
                         for fmt = format-list then (or (cdr fmt) fmt)
                         for sub-scan = (s-scan (car fmt) token type)
                         when sub-scan
                         collect sub-scan)
                (or type 'list)))))

;;;; List Functions

(defun sbr-pairlis (keys values &optional alist)
  (if (and (consp values) (= (length keys) (length values)))
      (cl-pairlis keys values alist)
    (cl-pairlis keys (make-list (length keys) values))))

;;;; Sequence Functions

(defun seq-sum (&optional seq)
  "Return sum of SEQ or 0 if SEQ is empty."
  (seq-reduce #'+ seq 0))

(defun seq-product (&optional seq)
  "Return sum of SEQ or 0 if SEQ is empty."
  (seq-reduce #'* seq 1))


(defun seq-add (&rest seqs)
  "Compute the entry-wise sums of SEQS."
  (let ((result  (apply #'seq-mapn #'+ seqs))
        (type1 (type-of (car seqs))))
    (if (eq type1 'cons)
        result
      (seq-into result type1))))

;; FIXME! returns a list due to collect
(defun sbr-seq-min (&rest seqs)
  (if (<= (length seqs) 1)
      (apply #'seq-min args)
    (cl-loop for col = seqs then (mapcar #'cdr col) while (car col)
             collect (seq-min (mapcar #'car col)))))

;; FIXME! returns a list due to collect
(defun sbr-seq-max (&rest seqs)
  (if (<= (length seqs) 1)
      (apply #'seq-max seqs)
    (cl-loop for col = seqs then (mapcar #'cdr col) while (car col)
             collect (seq-max (mapcar #'car col)))))

(defun seq-is-subset (seq1 seq2)
  "Return t if SEQ1 is a proper subset of SEQ2."
  (and (seq-intersection seq1 seq2)
       (not (seq-difference seq1 seq2))))

;;;; Vector Functions

(defun vector-index (vec indices)
  (vconcat (cl-loop for i in indices
                    collect (aref vec i))))

(defun vector-every-p (vec &optional indices testfn)
  (seq-every-p (or testfn #'identity)
               (if indices
                   (vector-index vec indices)
                 vec)))

(defun vector-all-zero-p (vec &optional indices)
  (vector-every-p vec indices #'zerop))

;; Need to do make-vector in a loop, because if used in make-list, then all
;; entries of the list will point to the same vector
(defun make-vector2d (height width init)
  (vconcat (cl-loop for n below height
                    collect (make-vector width init))))

(defun aref2d (a x y)
  (aref (aref a y) x))

;; Make aref2d setf-able
(gv-define-setter aref2d (val a x y) `(setf (aref (aref ,a ,y) ,x) ,val))

;;;; Hash Table Functions

(defun ht-inc (table key &optional x)
  (if (ht-get table key)
      (cl-incf (ht-get table key) (or x 1))
    (ht-set table key (or x 1))))

;;;; Footer

(provide 'aoc-helpers)

;;; aoc-helpers.el ends here
