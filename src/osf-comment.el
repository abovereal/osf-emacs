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

(defvar osf-evil-comment-style-determiner
  '(((mode . c-mode) . osf-evil-comment-c-cpp-style-determiner)
    ((mode . c++-mode) . osf-evil-comment-c-cpp-style-determiner))
  "A alist to determine the comment style used to comment or uncomment.
Each element is of the form (MODE . STYLE-DETERMINER).
MODE can be a cons of the form (mode . MAJOR-MODE), the car is the
symbol 'mode, the cdr specify the major-mode for the STYLE-DETERMINER
to be used.
MODE can also be a function, the function will be called with 0
argument in a buffer and it should return non-nil if the
STYLE-DETERMINER should be used.
STYLE-DETERMINER should be a function, it receive an argument COMMENT
which is a function, it should setup the comment style variables
bindings (`comment-use-syntax', `comment-column', `comment-start',
`comment-end', `comment-start-skip', etc) and call the COMMENT
function.")

(evil-define-operator osf-evil-comment-or-uncomment (beg end type)
  "Comment or uncomment the region."
  :move-point nil
  (let ((style-determiner
         (cl-some (lambda (mode-style-determiner)
                    (let ((mode (car mode-style-determiner))
                          (style-determiner (cdr mode-style-determiner)))
                      (cond ((and (consp mode)
                                  (eq (car mode) 'mode)
                                  (eq (cdr mode) major-mode))
                             style-determiner)
                            ((functionp mode)
                             (when (funcall mode) style-determiner)))))
                  osf-evil-comment-style-determiner)))
    (if style-determiner
        (funcall style-determiner
                 (lambda () (comment-or-uncomment-region beg end)))
      (comment-or-uncomment-region beg end))))

(defun osf-evil-comment-c-cpp-style-determiner (comment)
  (cond
   ((evil-visual-state-p)
    (cond ((or (eq (evil-visual-type) 'line)
               (save-mark-and-excursion
                 (and
                  (progn
                    (goto-char evil-visual-beginning)
                    (looking-back (rx line-start (* whitespace))))
                  (or (eq (char-before evil-visual-end) ?\n)
                      (progn
                        (goto-char evil-visual-end)
                        (looking-at (rx (* whitespace) line-end)))))))
           (let ((comment-start "//")
                 (comment-end ""))
             (funcall comment)))
          (t
           (let ((comment-start "/*")
                 (comment-end "*/"))
             (funcall comment)))))
   (t (funcall comment))))

(osf-evil-define-key 'normal 'global
  "g c" #'osf-evil-comment-or-uncomment)

(provide 'osf-comment)
