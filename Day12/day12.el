;;;; Header

(setq filename "input.txt"
      max-lisp-eval-depth 2000
      max-specpdl-size 10000)


(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))


;;;; Task Functions

(defun graph-add (node1 node2 graph &optional bidir)
  (let ((children (or (gethash node1 graph)
                      (puthash node1 (make-hash-table :test 'equal) graph))))
    (puthash node2 t children))
  (when bidir
    (graph-add node2 node1 graph nil)))

(defun print-graph (graph)
  (let ((lines nil))
    (maphash (lambda (key val)
               (push (format "%s --> %s" key
                             (string-join (hash-table-keys val) ",")) lines))
             graph)
    (string-join lines "\n")))

(defun make-graph (edges bidir &optional debug)
  (cl-loop for (node1 node2) in edges
           with graph = (make-hash-table :test #'equal)
           do (graph-add node1 node2 graph bidir)
           finally return (progn (debugmsg "%s" (print-graph graph)) graph)))

(defun has-lowercase-dups (lst)
  (and (consp lst)
       (or (and (s-lowercase? (car lst)) (member (car lst) (cdr lst)))
           (has-lowercase-dups (cdr lst)))))

(defun print-path (path)
  (string-join (reverse (cons "end" path)) ","))

(defun get-path-count (node path graph &optional debug part2)
  (cond ((equal node "end")
         (progn (debugmsg "%s" (print-path path)) 1))
        ((and (s-lowercase? node) (member node path)
              (or (not part2)
                  (and part2 (or (equal node "start")
                                 (has-lowercase-dups path)))))
         0)
        (t (cl-loop for child hash-keys of (gethash node graph)
                    sum (get-path-count child (cons node path)
                                        graph debug part2)))))

(defun day12 (&optional file part2 debug)
  (let* ((data (aoc-input file))
         (edges (mapcar (lambda (line) (split-string line "-")) data))
         (graph (make-graph edges t debug)))
    (get-path-count "start" nil graph debug part2)))


;;;; Notes

;; * How to initialize hash-table with dynamic values?
;; * Are hash-tables slower than lists for smaller lengths?

;;;; Old Task Functions



(defun graph-add-old (node1 node2 graph)
  (if-let ((children (gethash node1 graph)))
      (puthash node1 (cl-pushnew node2 children :test #'equal) graph)
    (puthash node1 (list node2) graph)))

(defun make-graph-old (data &optional debug)
  (cl-loop for line in data with graph = (make-hash-table :test #'equal)
           do (seq-let (node1 node2) (split-string line "-")
                (graph-add-old node1 node2 graph)
                (graph-add-old node2 node1 graph))
           finally return (progn
                            (when debug
                              (maphash (lambda (key val)
                                         (message "%s --> %S" key val)) graph))
                            graph)))

(defun has-lc-dups-old (lst)
  (cl-loop for (first . rest) on lst
           when (and (s-lowercase? first) (member first rest))
           return t))

(defun get-path-count-old (node current-path graph &optional debug part2)
  (cond ((equal node "end")
         (progn
           (debugmsg "%s" (string-join (reverse current-path) ","))
           1))
        ((and (s-lowercase? node)
              (member node current-path)
              (or (not part2)
                  (and part2 (or (equal node "start")
                                 (has-lc-dups-old current-path)))))
         0)
        (t (cl-loop for child in (gethash node graph)
                    sum (get-path-count-old child (cons node current-path)
                                           graph debug part2)))))

(defun day12-old (&optional file part2 debug)
  (let* ((data (aoc-input file))
         (graph (make-graph-old data debug)))
    (get-path-count-old "start" nil graph debug part2)))
