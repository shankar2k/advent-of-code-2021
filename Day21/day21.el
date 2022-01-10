;;;; Header

(setq lexical-binding t
      max-lisp-eval-depth 8000
      max-specpdl-size 20000)

(require 'aoc-helpers (expand-file-name "../aoc-helpers.el"))

;;;; Task Functions

(defvar die-value 1)

(defvar die-count 0)

(defun roll-die ()
  (let ((val die-value))
    (setq die-value (1+ (% die-value 100)))
    (cl-incf die-count)
    val))

(defun add-wrap (x y base)
  (1+ (% (+ (1- x) y) base)))

(defun day21-part1 (&optional file iter-count debug)
  (let* ((positions (aoc-input file
                               '(newline "Player %*d starting position: %d")))
         (winner nil))
    (setq die-value 1
          die-count 0)
    (setq scores '(0 0))
    (while (and (not winner) (or (not iter-count) (< die-count iter-count)))
      (cl-loop for ppos in-ref positions
               for player from 1 to 2
               for pscore in-ref scores
               do (let* ((rolls (cl-loop for n below 3
                                         collect (roll-die)))
                         (roll (seq-sum rolls))
                         (newpos (1+ (% (+ (1- roll) ppos) 10)))
                         (newscore (+ pscore newpos)))
                    (debugmsg "%d: Player %d rolls %S=%d and moves from %d --> %d and score %d --> %d."
                              die-count player rolls roll
                              ppos newpos pscore newscore)
                    (setf ppos newpos)
                    (setf pscore (min newscore 1000))
                    (setq winner (>= pscore 1000)))
               when winner
               return scores))
    (list die-count scores (* (- (seq-sum scores) 1000) die-count))))

(defun print-grid (grid)
  (cl-loop for state hash-keys of grid using (hash-values count)
           for n = 1 then (add-wrap n 1 7)
           concat (format "%S -> %d " state count)
           when (= n 7)
           concat "\n"))

(defun simulate-universes (state player player-grid new-grid wins)
    (cl-loop for uni-count in '(1 3 6 7 6 3 1)
             for roll from 3 to 9
             do
             (let* ((newpos (add-wrap roll (aref state player) 10))
                    (newscore (+ (aref state (+ player 2))  newpos))
                    (oldcount (ht-get player-grid state 0))
                    (newcount (* uni-count oldcount)))
               (if (>= newscore 21)
                   (cl-incf (aref wins player) newcount)
                 (when (> newcount 0)
                   (setq new-state (copy-sequence state))
                   (setf (aref new-state player) newpos)
                   (setf (aref new-state (+ player 2)) newscore)
                   (ht-inc new-grid new-state newcount))))))

(defun day21-part2 (&optional file iter-count debug)
  (let* ((positions (aoc-input file
                               '(newline "Player %*d starting position: %d")
                               'vector))
         (player-grid (ht-create))
         (new-grid (ht-create))
         (iter 0))
    (setq player-wins [0 0])
    (setf (aref player-wins 0) 0)
    (setf (aref player-wins 1) 0)
    (ht-inc player-grid (vconcat positions [0 0]))
    (debugmsg "Initial State:%s, Wins: %S\n"
              (print-grid player-grid) player-wins)
    (while (and (not (ht-empty? player-grid))
                (or (not iter-count) (< iter iter-count)))
      (cl-incf iter)
      (dotimes (player 2)
        (dotimes (pos2 10)
          (dotimes (pos1 10)
            (dotimes (score2 21)
              (dotimes (score1 21)
                (let ((state (vector (1+ pos1) (1+ pos2) score1 score2)))
                  (simulate-universes state player player-grid
                                      new-grid player-wins))))))
        (setq player-grid new-grid)
        (setq new-grid (ht-create))
        (debugmsg "After turn %d player %d, Branches: %d, Wins: %S\n"
                  iter player (ht-size player-grid) player-wins)
        (when (and (numberp debug) (>= debug 2))
          (message "State:\n%s\n"  (print-grid player-grid)))))
    player-wins))

;;;; Notes

;;;; Old Task Functions
