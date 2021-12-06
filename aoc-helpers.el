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

;;;; Input / Loading  Functions

(defun aoc-input (&optional file type delim)
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


;;;; Sequence Functions

(defun seq-sum (seq)
  (seq-reduce #'+ seq 0))

(defun seq-add (&rest seqs)
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
             collect (apply #'seq-min (mapcar #'car col)))))

;; FIXME! returns a list due to collect
(defun sbr-seq-max (&rest seqs)
  (if (<= (length seqs) 1)
      (apply #'seq-max args)
    (cl-loop for col = seqs then (mapcar #'cdr col) while (car col)
             collect (apply #'seq-max (mapcar #'car col)))))

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

;;;; Footer

(provide 'aoc-helpers)

;;; aoc-helpers.el ends here



