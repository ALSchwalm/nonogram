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

;; Based on: blackbox.el

;;; Code:

(defvar nonogram-rows 10)
(defvar nonogram-columns 10)
(defvar nonogram-errors 0)
(defvar nonogram-max-erros 5)
(defvar nonogram-pos-x -1)
(defvar nonogram-pos-y -1)
(defvar nonogram-points nil)

(make-variable-buffer-local 'nonogram-rows)
(make-variable-buffer-local 'nonogram-columns)
(make-variable-buffer-local 'nonogram-errors)
(make-variable-buffer-local 'nonogram-pos-x)
(make-variable-buffer-local 'nonogram-pox-y)
(make-variable-buffer-local 'nonogram-points)
(make-variable-buffer-local 'nonogram-max-errors)

(defun nonogram-draw-board ()
  "Draw the board on which the game will be played."
  (erase-buffer)
  (let ((i nonogram-columns)
        (j nonogram-rows)
        (buffer-read-only nil))
    (while (>= (setq j (1- j)) 0)
      (while (>= (setq i (1- i)) 0)
        (if (eq i 0)
            (insert "-")
          (insert "- ")))
      (setq i nonogram-columns)
      (if (eq j 0)
          ()
        (insert "\n")))))

(defun nonogram-generate-points (num-points)
  "Fill the board with initial points.
NUM-POINTS: the number of points on the board"
  (let (point)
    (while (>= (setq num-points (1- num-points)) 0)
      (while
          (progn
            (setq point (cons (random 8) (random 8)))
            (member point nonogram-points)))
      (setq nonogram-points (cons point nonogram-points)))
    nonogram-points))

(defun nonogram-select ()
  "Select the current location as a point."
  (interactive)
  (let ((point (cons nonogram-pos-x nonogram-pos-y))
        (buffer-read-only nil))
    (if (member point nonogram-points)
        (progn (delete-char 1) (insert "0"))
      (message "Incorrect")
      (delete-char 1)
      (insert "O")
      (setq nonogram-errors (1+ nonogram-errors))))
  (backward-char))

(defun nonogram-generate-row-string (row-number)
  "Generate a string representing the points on the given row.
ROW-NUMBER: the row"
  (let ()))

(defun nonogram-right ()
  "Move nonogram cursor right."
  (interactive)
  (if (or (eq nonogram-pos-x -1)
          (eq nonogram-pos-x nonogram-columns))
      ()
    (forward-char 2)
    (setq nonogram-pos-x (1+ nonogram-pos-x))))

(defun nonogram-left ()
  "Move nonogram cursor left."
  (interactive)
  (if (or (eq nonogram-pos-x -1)
          (eq nonogram-pos-x 1))
      ()
    (backward-char 2)
    (setq nonogram-pos-x (1- nonogram-pos-x))))

(defun nonogram-up ()
  "Move nonogram cursor up."
  (interactive)
  (if (or  (eq nonogram-pos-y -1)
           (eq nonogram-pos-y 1))
      ()
    (with-no-warnings (previous-line))
    (setq nonogram-pos-y (1- nonogram-pos-y))))

(defun nonogram-down ()
  "Move nonogram cursor down."
  (interactive)
  (if (or (eq nonogram-pos-y -1)
          (eq nonogram-pos-y nonogram-rows))
      ()
    (with-no-warnings (next-line))
    (setq nonogram-pos-y (1+ nonogram-pos-y))))

(defun nonogram-eol ()
  "Move nonogram cursor to the end of the line."
  (interactive)
  (end-of-line)
  (backward-char 1)
  (setq nonogram-pos-x nonogram-columns))

(defun nonogram-bol ()
  "Move nonogram cursor to the beginning of the line."
  (interactive)
  (beginning-of-line)
  (setq nonogram-pos-x 1))

(defun nonogram-start ()
  "Initialize a buffer for nonogram play."
  (switch-to-buffer "*nonogram*")
  (nonogram-mode)
  (nonogram-draw-board)
  (nonogram-generate-points nonogram-columns)
  (setq nonogram-pos-x 1)
  (setq nonogram-pos-y 1)
  (with-no-warnings (beginning-of-buffer)))

(defun nonogram ()
  "Play a game of nonogram."
  (interactive)
  (nonogram-start))

(defvar nonogram-mode-map nil)
(if nonogram-mode-map
    ()
  (setq nonogram-mode-map (make-keymap))
  (suppress-keymap nonogram-mode-map t)
  (define-key nonogram-mode-map "\C-f" 'nonogram-right)
  (define-key nonogram-mode-map "\C-b" 'nonogram-left)
  (define-key nonogram-mode-map "\C-p" 'nonogram-up)
  (define-key nonogram-mode-map "\C-n" 'nonogram-down)
  (define-key nonogram-mode-map "\C-e" 'nonogram-eol)
  (define-key nonogram-mode-map "\C-a" 'nonogram-bol)
  (define-key nonogram-mode-map " " 'nonogram-select)
  (define-key nonogram-mode-map "q" 'bury-buffer))

(define-derived-mode nonogram-mode special-mode "nonogram"
  "A mode for playing nonogram."
  (use-local-map nonogram-mode-map)
  (setf show-trailing-whitespace nil))

;;; nonogram.el ends here
