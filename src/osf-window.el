;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2026 Chen Zhexuan

;; Author: Chen Zhexuan
;; URL: https://github.com/CloseToZero/osf-emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defun osf-mru-window ()
  (get-mru-window nil t t nil))

(defun osf-select-mru-window ()
  (interactive)
  (when-let* ((mru-window (osf-mru-window)))
    (select-window mru-window)))

(defface osf-mru-window-mode-line-face
  '((t (:foreground "#8A2BE2")))
  "Face for the mru window indicator shown in the mode-line.")

(defvar-local osf-mru-window-mode-line "")
(defun osf-mru-window-mode-line ()
  (if (eq (window-parameter (selected-window) 'osf-mru-window) (get-buffer-window))
      (propertize " MRU " 'face 'osf-mru-window-mode-line-face)
    ""))

(defun osf-add-mru-window-mode-line ()
  (let ((mru-window-mode-line (list :eval '(osf-mru-window-mode-line))))
    (setq-default mode-line-front-space
                  (let ((x (or (default-value 'mode-line-front-space) '(""))))
                    (if (and (listp x) (stringp (car x)) (string= (car x) ""))
                        x
                      (list "" x))))
    (setq-default mode-line-front-space (delete mru-window-mode-line mode-line-front-space))
    (setq-default mode-line-front-space
                  (cons (car mode-line-front-space)
                        (cons mru-window-mode-line (cdr mode-line-front-space))))))

(defun osf-update-mru-window-mode-line (window mru-window)
  (with-selected-window window
    (set-window-parameter window 'osf-mru-window mru-window)
    (force-mode-line-update)))

(defun osf-update-mru-window-mode-line-all-windows (&rest args)
  (let ((mru-window (osf-mru-window)))
    (dolist (window (window-list))
      (with-selected-window window
        (set-window-parameter window 'osf-mru-window mru-window)))
    (force-mode-line-update t)))

(osf-add-mru-window-mode-line)
(add-hook 'window-selection-change-functions #'osf-update-mru-window-mode-line-all-windows)

(straight-use-package 'ace-window)
(setq aw-dispatch-always t)
(when (fboundp 'posframe-show)
  (with-eval-after-load 'ace-window
    (ace-window-posframe-mode)))

(winner-mode)

(straight-use-package 'hydra)

(defvar osf-default-resize-window-step 4)

(straight-use-package 'buffer-move)

(require 'windmove)

(defun osf-resize-window-step (prefix)
  (if prefix
      (prefix-numeric-value prefix)
    osf-default-resize-window-step))

(defun osf-move-splitter-left (arg)
  "Move the window splitter left."
  (interactive "P")
  (let ((resize-step (osf-resize-window-step arg)))
    (if (let ((windmove-wrap-around nil))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally resize-step)
      (enlarge-window-horizontally resize-step))))

(defun osf-move-splitter-right (arg)
  "Move the window splitter right."
  (interactive "P")
  (let ((resize-step (osf-resize-window-step arg)))
    (if (let ((windmove-wrap-around nil))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally resize-step)
      (shrink-window-horizontally resize-step))))

(defun osf-move-splitter-up (arg)
  "Move the window splitter up."
  (interactive "P")
  (let ((resize-step (osf-resize-window-step arg)))
    (if (let ((windmove-wrap-around nil))
          (windmove-find-other-window 'up))
        (enlarge-window resize-step)
      (shrink-window resize-step))))

(defun osf-move-splitter-down (arg)
  "Move the window splitter down."
  (interactive "P")
  (let ((resize-step (osf-resize-window-step arg)))
    (if (let ((windmove-wrap-around nil))
          (windmove-find-other-window 'up))
        (shrink-window resize-step)
      (enlarge-window resize-step))))

(defhydra osf-manage-window (:color red :hint nil)
  "
Window Move: _h_ left  _l_ right  _j_ down  _k_ up
Buffer Move: _C-h_ left  _C-l_ right  _C-j_ down  _C-k_ up
     Resize: _H_ left  _L_ right  _J_ down  _K_ up
Resize Step: _1_ _2_ _3_ _4_ _5_  current step: %`osf-default-resize-window-step
       Quit: _q_ quit"
  ("h" windmove-left)
  ("l" windmove-right)
  ("j" windmove-down)
  ("k" windmove-up)
  ("C-h" buf-move-left)
  ("C-l" buf-move-right)
  ("C-j" buf-move-down)
  ("C-k" buf-move-up)
  ("H" osf-move-splitter-left)
  ("L" osf-move-splitter-right)
  ("J" osf-move-splitter-down)
  ("K" osf-move-splitter-up)
  ("1" (lambda () (interactive) (setq osf-default-resize-window-step 1)))
  ("2" (lambda () (interactive) (setq osf-default-resize-window-step 2)))
  ("3" (lambda () (interactive) (setq osf-default-resize-window-step 3)))
  ("4" (lambda () (interactive) (setq osf-default-resize-window-step 4)))
  ("5" (lambda () (interactive) (setq osf-default-resize-window-step 5)))
  ("q" nil))

(defun osf-evil-window-split (&optional arg)
  (interactive "P")
  (cond (arg
         (select-window
          (split-window (frame-root-window) nil
                        (if evil-split-window-below 'below 'above)))
         (when evil-auto-balance-windows (balance-windows (window-parent))))
        (t (evil-window-split))))

(defun osf-evil-window-split-below-if-root (&optional arg)
  (interactive "P")
  (cond (arg
         (split-window (frame-root-window) nil 'below)
         (when evil-auto-balance-windows (balance-windows (window-parent))))
        (t (evil-window-split))))

(defun osf-evil-window-vsplit (&optional arg)
  (interactive "P")
  (cond (arg
         (select-window
          (split-window (frame-root-window) nil
                        (if evil-split-window-right 'right 'left)))
         (when evil-auto-balance-windows (balance-windows (window-parent))))
        (t (evil-window-vsplit))))

(straight-use-package 'zoom-window)
(with-eval-after-load 'zoom-window
  (custom-set-variables '(zoom-window-mode-line-color "LightBlue")))

(osf-leader-define-key 'global
  "w" evil-window-map
  "w -" #'osf-evil-window-split-below-if-root
  "w \\" #'osf-evil-window-vsplit
  "w a" #'ace-window
  "w p" #'osf-select-mru-window
  "w u" #'winner-undo
  "w x" #'evil-window-delete
  "w C-r" #'winner-redo
  "w M" #'osf-manage-window/body
  "w z" #'zoom-window-zoom)

(provide 'osf-window)
