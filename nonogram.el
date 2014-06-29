;;; nonogram.el --- implementation of nonogram for GNU Emacs

;; Copyright (c) 2014 Adam Schwalm
;; Author: Adam Schwalm <adamschwalm@gmail.com>
;; Version: 0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Nonograms are a type of logic puzzle in which numeric clues must be used
;; to determine which of a grid of boxes should be 'filled'.

;; For example, consider the following puzzle:
;;                 2   2
;;               2 1 1 1 3
;;             1 - - - - -
;;           1 1 - - - - -
;;           1 2 - - - - -
;;           2 1 - - - - -
;;             3 - - - - -

;; The dashes constitute the grid mentioned above.  Each row of numbers to the
;; left of the grid is a hint for solving that row of the grid.  Similarly, each
;; column of numbers above the grid is a hint for solving the corresponding grid
;; column.  The hints may be interpreted in the following way:

;; Each number indicates a grouping of filled boxes in the row or column.  If there
;; are multiple numbers, then there are multiple groupings of filled boxes separated
;; by at least one empty box.  So for example, the hint for the first row in the
;; above puzzle is '1'.  Which means there is a group of exactly one filled box
;; in that row of the grid.  More concisely, there is one filled box somewhere on
;; that row.  The hint for the next row (1 1), indicates that there are two filled
;; boxes on that row with at least one space between them.

;; By examining the clues for each row and column, it may be determined that the
;; solution to the above puzzle is:

;;                 2   2
;;               2 1 1 1 3
;;             1 x 0 x x x
;;           1 1 x 0 x 0 x
;;           1 2 0 x x 0 0
;;           2 1 0 0 x x 0
;;             3 x x 0 0 0

;; Where '0' represents a filled box and 'x' represents an empty box.

;; Based on: blackbox.el

;;; Code:

(require 'dash)

(defvar nonogram-rows 5)
(defvar nonogram-columns 5)
(defvar nonogram-correct 0)
(defvar nonogram-errors 0)
(defvar nonogram-max-erros 5)
(defvar nonogram-pos-x -1)
(defvar nonogram-pos-y -1)
(defvar nonogram-start-pos '(-1 -1))
(defvar nonogram-points nil)

(defun nonogram-draw-column-hints (column-hints max-column-hint-length)
  "Draw the hints for the current game.
COLUMN-HINTS: List of column hints
MAX-COLUMN-HINT-LENGTH: Length of the longest column hint"
  (let (column-hint)
    (-dotimes max-column-hint-length
      (lambda (row)
        (-dotimes nonogram-columns
          (lambda (column)
            (setq column-hint (nth column column-hints))
            (while (< (length column-hint) max-column-hint-length)
              (setq column-hint (cons nil column-hint)))
            (if (nth row column-hint)
                (insert (concat (number-to-string
                                 (nth row column-hint)) " "))
              (insert "  "))))
        (next-line)))))

(defun nonogram-draw-board ()
  "Draw the board on which the game will be played."
  (let ((i nonogram-columns)
        (j nonogram-rows)
        (buffer-read-only nil)
        (column-hints ())
        (row-hints ())
        max-row-hint-length
        max-column-hint-length
        row-strings
        column-strings)
    (erase-buffer)
    (-dotimes nonogram-columns
      (lambda (n) (setq column-hints (cons (nonogram-generate-hint 'column (1+ n))
                                           column-hints))))

    (-dotimes nonogram-rows
      (lambda (n) (setq row-hints (cons (nonogram-generate-hint 'row (1+ n))
                                        row-hints))))

    (setq row-strings (--map (if it (substring (format "%s" it) 1 -1)
                               "") row-hints))

    (setq max-row-hint-length (length (--max-by (> (length it)
                                                   (length other)) row-strings)))

    (setq max-column-hint-length (length (--max-by (> (length it)
                                                      (length other)) column-hints)))

    (--dotimes max-column-hint-length
      (insert (concat (make-string (1+ max-row-hint-length) ? ) "\n")))
    (backward-char)
    (previous-line (1- max-column-hint-length))
    (nonogram-draw-column-hints (reverse column-hints) max-column-hint-length)

    (while (>= (setq j (1- j)) 0)
      (insert (format (format "%%%ds " max-row-hint-length)
                      (nth j row-strings)))
      (while (>= (setq i (1- i)) 0)
        (if (eq i 0)
            (insert "-")
          (insert "- ")))
      (setq i nonogram-columns)
      (if (eq j 0)
          ()
        (insert "\n")))
    (list (1+ max-row-hint-length) max-column-hint-length)))

(defun nonogram-generate-points (num-points)
  "Fill the board with initial points.
NUM-POINTS: the number of points on the board"
  (let (point)
    (while (>= (setq num-points (1- num-points)) 0)
      (while (progn
               (setq point (cons (1+ (random nonogram-columns))
                                 (1+ (random nonogram-rows))))
               (member point nonogram-points)))
      (setq nonogram-points (cons point nonogram-points)))
    nonogram-points))

(defun nonogram-select ()
  "Select the current location as a point."
  (interactive)
  (if (eq (following-char) ?-)
      (let ((point (cons nonogram-pos-x nonogram-pos-y))
            (buffer-read-only nil))
        (save-excursion
          (if (member point nonogram-points)
              (progn
                (delete-char 1)
                (insert "0")
                (setq nonogram-correct (1+ nonogram-correct))
                (if (eq nonogram-correct (length nonogram-points))
                    (nonogram-game-over 'win)))
            (message "Incorrect")
            (delete-char 1)
            (insert "X")
            (setq nonogram-errors (1+ nonogram-errors))
            (nonogram-update-score)
            (if (eq nonogram-errors nonogram-max-erros)
                (nonogram-game-over 'lose)))))))

(defun nonogram-give-up ()
  "Give up and reveal the board."
  (interactive)
  (nonogram-reveal-board)
  (nonogram-game-over 'lose))

(defun nonogram-game-over (win-or-lose)
  "End the current game and display score.
WIN-OR-LOSE: whether the player has won or lost"
  (let ((buffer-read-only nil))
    (if (eq win-or-lose 'win)
        (message "Game over. Nice job!")
      (message "Game over. Better luck next time."))
    (setq nonogram-pos-x -1)
    (setq nonogram-pos-y -1)))

(defun nonogram-reveal-board ()
  "Reveal the hidden board state to the player."
  (let ((buffer-read-only nil))
    (beginning-of-buffer)
    (next-line (cadr nonogram-start-pos))
    (forward-char (car nonogram-start-pos))
    (setq nonogram-pos-x 1)
    (setq nonogram-pos-y 1)

    (-dotimes nonogram-rows
      (lambda (count)
        (-dotimes nonogram-columns
          (lambda (count)
            (if (member (cons nonogram-pos-x nonogram-pos-y)
                        nonogram-points)
                (progn (delete-char 1) (insert "0"))
              (delete-char 1)
              (insert "x"))
            (backward-char)
            (nonogram-right)))
        (nonogram-down)
        (nonogram-bol)))))

(defun nonogram-mark-empty (&optional notoggle)
  "Mark the current location as empty.
With 'NOTOGGLE' only mark the space as empty, instead of toggling."
  (interactive "p")
  (save-excursion
    (let ((buffer-read-only nil)
          (case-fold-search nil)
          (notoggle (if notoggle notoggle nil)))

      (if (not (region-active-p))
          (cond ((eq (following-char) ?-)
                 (delete-char 1)
                 (insert "x"))
                ((if (and (eq (following-char) ?x)) (not notoggle))
                 (delete-char 1)
                 (insert "-")))
        (if (> (count-matches "-" (region-beginning) (1+ (region-end))) 0)
            (replace-string "-" "x" nil (region-beginning) (1+ (region-end)))
          (replace-string "x" "-" nil (region-beginning) (1+ (region-end))))))))

(defun nonogram-hint-from-points (points)
  "Create a hint given a series of numbers.
POINTS: list of numbers representing rows or columns containing points."
  (if (not points)
      nil
    (let ((previous-value (car points)) (hint '(1)))
      (while points
        (if (= (car points) (1+ previous-value))
            (setcar hint (1+ (car hint)))
          (setq hint (cons 1 hint)))
        (setq previous-value (car points))
        (setq points (cdr points)))
      (reverse (butlast hint)))))

(defun nonogram-generate-hint (row-or-column number)
  "Generate a string representing the points on the given row.
ROW-OR-COLUMN: whether the hint should be build by row or column
NUMBER: which row or column to use"
  (let (filter-func value-func points values)
    (if (eq row-or-column 'column)
        (setq filter-func 'car
              value-func 'cdr)
      (setq filter-func 'cdr
            value-func 'car))

    (setq points (-filter (lambda (point)
                            (= (funcall filter-func point) number))
                          nonogram-points))

    (setq values (sort (-map value-func points) '<))
    (nonogram-hint-from-points values)))

(defun nonogram-update-score ()
  "Update the displayed score (number of mistakes)."
  (let ((buffer-read-only nil))
    (with-no-warnings (end-of-buffer))
    (beginning-of-line)
    (kill-line)
    (insert (format "   Errors: %d/%d" nonogram-errors nonogram-max-erros))))

(defun nonogram-mark-row-empty ()
  "Mark the current row as empty."
  (interactive)
  (let ((nonogram-pos-x nonogram-pos-x))
    (save-excursion
      (nonogram-bol)
      (set-mark (point))
      (nonogram-eol)
      (nonogram-mark-empty))))

(defun nonogram-mark-column-empty ()
  "Mark the current column empty."
  (interactive)
  (let ((nonogram-pos-y nonogram-pos-y)
        (do-mark-empty nil))
    (save-excursion
      (nonogram-up nonogram-rows)
      (--dotimes nonogram-rows
        (progn
          (if (eq (following-char) ?-)
             (setq do-mark-empty t))
          (nonogram-down)))
      (nonogram-up nonogram-rows)
      (--dotimes nonogram-rows
        (progn
          (if do-mark-empty
             (nonogram-mark-empty 'notoggle)
           (nonogram-mark-empty))
          (nonogram-down))))))

(defun nonogram-right (&optional N)
  "Move nonogram cursor N spaces right."
  (interactive "p")
  (let ((N (if N N 1)))
   (while (and (>= (setq N (1- N)) 0)
               (not (or (eq nonogram-pos-x -1)
                        (eq nonogram-pos-x nonogram-columns))))
     (forward-char 2)
     (setq nonogram-pos-x (1+ nonogram-pos-x)))))

(defun nonogram-left (&optional N)
  "Move nonogram cursor N spaces left."
  (interactive "p")
  (let ((N (if N N 1)))
    (while (and (>= (setq N (1- N)) 0)
                (not (or (eq nonogram-pos-x -1)
                         (eq nonogram-pos-x 1))))
      (backward-char 2)
      (setq nonogram-pos-x (1- nonogram-pos-x)))))

(defun nonogram-up (&optional N)
  "Move nonogram cursor N spaces up."
  (interactive "p")
  (let ((N (if N N 1)))
    (while (and (>= (setq N (1- N)) 0)
                (not (or (eq nonogram-pos-y -1)
                         (eq nonogram-pos-y 1))))
      (with-no-warnings (previous-line))
      (setq nonogram-pos-y (1- nonogram-pos-y)))))

(defun nonogram-down (&optional N)
  "Move nonogram cursor N spaces down."
  (interactive "p")
  (let ((N (if N N 1)))
    (while (and (>= (setq N (1- N)) 0)
                (not (or (eq nonogram-pos-y -1)
                         (eq nonogram-pos-y nonogram-rows))))
      (with-no-warnings (next-line))
      (setq nonogram-pos-y (1+ nonogram-pos-y)))))

(defun nonogram-eol ()
  "Move nonogram cursor to the end of the line."
  (interactive)
  (unless (eq nonogram-pos-x -1)
    (end-of-line)
    (backward-char 1)
    (setq nonogram-pos-x nonogram-columns)))

(defun nonogram-bol ()
  "Move nonogram cursor to the beginning of the line."
  (interactive)
  (unless (eq nonogram-pos-x -1)
    (beginning-of-line)
    (forward-char (car nonogram-start-pos))
    (setq nonogram-pos-x 1)))

(defun nonogram-start ()
  "Initialize a buffer for nonogram play."
  (switch-to-buffer "*nonogram*")
  (setq buffer-read-only t)
  (buffer-disable-undo (current-buffer))
  (nonogram-mode)
  (nonogram-generate-points (floor (* nonogram-columns nonogram-rows 0.5)))
  (let ((buffer-read-only nil))
    (setq nonogram-start-pos (nonogram-draw-board))
    (with-no-warnings (end-of-buffer))
    (newline 3)
    (insert (format "   Errors: %d/%d" nonogram-errors nonogram-max-erros)))
  (with-no-warnings (beginning-of-buffer))
  (forward-char (car nonogram-start-pos))
  (next-line (cadr nonogram-start-pos)))

(defun nonogram (&optional rows columns)
  "Generate a nonogram.
ROWS: Number of rows in the generated puzzle
COLUMNS: Number of columns in the generated puzzle

Nonograms are a type of logic puzzle in which numeric clues must be used
to determine which of a grid of boxes should be 'filled'.

For example, consider the following puzzle:
                2   2
              2 1 1 1 3
            1 - - - - -
          1 1 - - - - -
          1 2 - - - - -
          2 1 - - - - -
            3 - - - - -

The dashes constitute the grid mentioned above.  Each row of numbers to the
left of the grid is a hint for solving that row of the grid.  Similarly, each
column of numbers above the grid is a hint for solving the corresponding grid
column.  The hints may be interpreted in the following way:
 Each number indicates a grouping of filled boxes in the row or column.  If
there are multiple numbers, then there are multiple groupings of filled boxes
separated by at least one empty box.  So for example, the hint for the first
row in the above puzzle is '1'.  Which means there is a group of exactly one
filled box in that row of the grid.  More concisely, there is one filled box
somewhere on that row.  The hint for the next row (1 1), indicates that there
are two filled boxes on that row with at least one space between them.

By examining the clues for each row and column, it may be determined that the
solution to the above puzzle is:

                2   2
              2 1 1 1 3
            1 x 0 x x x
          1 1 x 0 x 0 x
          1 2 0 x x 0 0
          2 1 0 0 x x 0
            3 x x 0 0 0

Where '0' represents a filled box and 'x' represents an empty box."
  (interactive)
  (if (eq major-mode 'nonogram-mode)
      (setq nonogram-rows (or rows nonogram-rows))
    (setq nonogram-rows (or rows 10)))
  (if (eq major-mode 'nonogram-mode)
      (setq nonogram-columns (or rows nonogram-columns))
    (setq nonogram-columns (or rows 10)))
  (setq nonogram-pos-x 1)
  (setq nonogram-pos-y 1)
  (setq nonogram-start-pos '(-1 -1))
  (setq nonogram-errors 0)
  (setq nonogram-correct 0)
  (setq nonogram-max-erros 5)
  (setq nonogram-points nil)
  (nonogram-start))

(defvar nonogram-mode-map nil)
(unless nonogram-mode-map
  (setq nonogram-mode-map (make-keymap))
  (suppress-keymap nonogram-mode-map t)
  (define-key nonogram-mode-map "\C-f" 'nonogram-right)
  (define-key nonogram-mode-map [right] 'nonogram-right)
  (define-key nonogram-mode-map "\C-b" 'nonogram-left)
  (define-key nonogram-mode-map [left] 'nonogram-left)
  (define-key nonogram-mode-map "\C-p" 'nonogram-up)
  (define-key nonogram-mode-map [up] 'nonogram-up)
  (define-key nonogram-mode-map "\C-n" 'nonogram-down)
  (define-key nonogram-mode-map [down] 'nonogram-down)
  (define-key nonogram-mode-map "\C-e" 'nonogram-eol)
  (define-key nonogram-mode-map "\C-a" 'nonogram-bol)
  (define-key nonogram-mode-map " " 'nonogram-select)
  (define-key nonogram-mode-map "f" 'nonogram-mark-empty)
  (define-key nonogram-mode-map "F" 'nonogram-mark-row-empty)
  (define-key nonogram-mode-map "\M-f" 'nonogram-mark-column-empty)
  (define-key nonogram-mode-map "q" 'bury-buffer)
  (define-key nonogram-mode-map "\C-r" 'nonogram-give-up)
  (define-key nonogram-mode-map "r" 'nonogram))

(define-derived-mode nonogram-mode special-mode "nonogram"
  "A mode for solving nonogram puzzles.
\\{nonogram-mode-map}"
  (kill-all-local-variables)
  (use-local-map nonogram-mode-map)
  (setq major-mode 'nonogram-mode)
  (setq mode-name "Nonogram")
  (setf show-trailing-whitespace nil))

(provide 'nonogram)
;;; nonogram.el ends here
