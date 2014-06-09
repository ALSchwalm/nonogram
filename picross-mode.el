;;; picross-mode.el --- implementation of picross for GNU Emacs

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

(defvar picross-rows 10)
(defvar picross-columns 10)
(defvar picross-errors 0)
(defvar picross-pos-x -1)
(defvar picross-pos-y -1)

(make-variable-buffer-local 'picross-rows)
(make-variable-buffer-local 'picross-columns)
(make-variable-buffer-local 'picross-errors)
(make-variable-buffer-local 'picross-pos-x)
(make-variable-buffer-local 'picross-pox-y)


(defun picross-draw-board ()
  "Draw the board on which the game will be played."
  (erase-buffer)
  (let ((i picross-columns)
        (j picross-rows)
        (buffer-read-only nil))
    (while (>= (setq j (1- j)) 0)
      (while (>= (setq i (1- i)) 0)
        (if (eq i 0)
            (insert "-")
          (insert "- ")))
      (setq i picross-columns)
      (if (eq j 0)
          ()
        (insert "\n")))))

(defun picross-right ()
  "Move picross cursor right."
  (interactive)
  (if (or (eq picross-pos-x -1)
          (eq picross-pos-x picross-columns))
      ()
    (forward-char 2)
    (setq picross-pos-x (1+ picross-pos-x))))

(defun picross-left ()
  "Move picross cursor left."
  (interactive)
  (if (or (eq picross-pos-x -1)
          (eq picross-pos-x 1))
      ()
    (backward-char 2)
    (setq picross-pos-x (1- picross-pos-x))))

(defun picross-up ()
  "Move picross cursor up."
  (interactive)
  (if (or  (eq picross-pos-y -1)
           (eq picross-pos-y 1))
      ()
    (with-no-warnings (previous-line))
    (setq picross-pos-y (1- picross-pos-y))))

(defun picross-down ()
  "Move picross cursor down."
  (interactive)
  (if (or (eq picross-pos-y -1)
          (eq picross-pos-y picross-rows))
      ()
    (with-no-warnings (next-line))
    (setq picross-pos-y (1+ picross-pos-y))))

(defun picross-eol ()
  "Move picross cursor to the end of the line."
  (interactive)
  (end-of-line)
  (backward-char 1)
  (setq picross-pos-x picross-columns))

(defun picross-bol ()
  "Move picross cursor to the beginning of the line."
  (interactive)
  (beginning-of-line)
  (setq picross-pos-x 1))

(defun picross-start ()
  "Initialize a buffer for picross play."
  (switch-to-buffer "*picross*")
  (picross-mode)
  (picross-draw-board)
  (setq picross-pos-x 1)
  (setq picross-pos-y 1)
  (with-no-warnings (beginning-of-buffer)))

(defun picross ()
  "Play a game of picross."
  (interactive)
  (picross-start))

(defvar picross-mode-map nil)
(if picross-mode-map
    ()
  (setq picross-mode-map (make-keymap))
  (suppress-keymap picross-mode-map t)
  (define-key picross-mode-map "\C-f" 'picross-right)
  (define-key picross-mode-map "\C-b" 'picross-left)
  (define-key picross-mode-map "\C-p" 'picross-up)
  (define-key picross-mode-map "\C-n" 'picross-down)
  (define-key picross-mode-map "\C-e" 'picross-eol)
  (define-key picross-mode-map "\C-a" 'picross-bol))

(define-derived-mode picross-mode special-mode "picross"
  "A mode for playing picross."
  (use-local-map picross-mode-map)
  (setf show-trailing-whitespace nil))

;;; picross-mode.el ends here
