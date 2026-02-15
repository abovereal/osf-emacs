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

;; TODOs
;; - Support osf-scratch-ignore-major-modes mechanism inside
;;   `osf-scratch-major-modes-fn-default'.
;; - Allow customize the content of the header comment.
;; - Add command to kill all scratch buffers (right now, we can use
;;   ibuffer instead).
;; - Add command to query and kill all scratch buffers.

(defvar osf-scratch-naming-fn #'osf-scratch-naming-fn-default
  "The function called to generate a name for a scratch buffer.

This function takes one argument: the major mode symbol.

By default, the existing scratch buffer with the same name will
be reused, you can specify your own naming function and use
`generate-new-buffer-name' to generate a unqiue name for every
scratch buffer to avoid reusing the existing scratch buffer.")

(defvar osf-scratch-major-modes-fn #'osf-scratch-major-modes-fn-default
  "The function called to return a list of known major mode symbols.
Each major mode symbol should be a function to enable the major mode.")

(defvar osf-scratch-major-mode-display-name-fn
  #'osf-scratch-major-mode-display-name-fn-default
  "The function called to return the display name (string) of a
known major mode (one of the major modes returned by
`osf-scratch-major-modes-fn').

This function takes one argument: a major mode symbol.

NOTE: This function should return an unqiue name for every distinct
major mode.")

(defvar osf-scratch-always-ask-major-mode t
  "Whether always ask a major mode for a scratch buffer or just
reuse the major mode of the current buffer.  Even the value of this
variable is non-nil, you can always select the major mode of the
current buffer by the completion default value.  Likewise, even the
value of this variable is nil, you can always specify a prefix
argument by \\[universal-argument] or \\[negative-argument] for
asking a major mode to start with.")

(defvar osf-scratch-insert-header-comment t
  "Whether insert header comment like \"Scratch buffer for emacs-lisp-mode\".
NOTE: when the major mode does not have comment syntax, not
header comment is inserted.")

(defvar-local osf-scratch-buffer? nil
  "Whether the current buffer is a scratch buffer.")

(defvar osf-scratch-create-buffer-hook nil
  "The hooks to run when creating a scratch buffer.")

(defvar osf-scratch-major-mode-history nil
  "The history of selected major modes.")
(osf-add-saved-vars 'osf-scratch-major-mode-history)

(defun osf-scratch-naming-fn-default (mode)
  "Generate name for a scratch buffer like \"*scratch:major-mode*\"."
  (concat "*scratch:" (symbol-name mode) "*"))

(defun osf-scratch-major-modes-fn-default ()
  "Return a list of known major modes."
  (let ((result nil))
    (obarray-map
     (lambda (symbol)
       (let ((symbol-name (symbol-name symbol)))
         (when (and (functionp symbol)
                    (not (member symbol minor-mode-list))
                    (string-match-p (rx "-mode" string-end) symbol-name)
                    (not (string-match-p "--" symbol-name)))
           (push symbol result))))
     obarray)
    (nreverse result)))

(defun osf-scratch-major-mode-display-name-fn-default (mode)
  (let ((mode-name (symbol-name mode)))
    (if (string-match-p (rx "-mode" string-end) mode-name)
        (substring mode-name 0 -5)
      mode-name)))

(defun osf-scratch (&optional mode)
  "Create a mode-specific scratch buffer.
MODE is the major mode."
  (interactive
   (list
    (if osf-scratch-always-ask-major-mode
        (let* ((major-modes (funcall osf-scratch-major-modes-fn))
               (selected-major-mode-display-name
                (completing-read
                 "Major mode for scratch buffer: "
                 (mapcar osf-scratch-major-mode-display-name-fn major-modes)
                 nil t nil 'osf-scratch-major-mode-history
                 (funcall osf-scratch-major-mode-display-name-fn major-mode))))
          (seq-find
           (lambda (major-mode-v) ; avoid warning about shadowing the variable `major-mode'.
             (string=
              (funcall osf-scratch-major-mode-display-name-fn major-mode-v)
              selected-major-mode-display-name))
           major-modes))
      major-mode)))
  (unless (functionp mode)
    (error "Unknown mode: %S" mode))
  (let* ((buffer-name (funcall osf-scratch-naming-fn mode))
         (new-buffer? (not (get-buffer buffer-name)))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (funcall mode)
      (when (and new-buffer? osf-scratch-insert-header-comment)
        (when comment-start
          (let ((temp-content "a"))
            (insert temp-content) ; temp content for restoring the current position.
            (save-excursion
              (beginning-of-buffer)
              (comment-region
               (point)
               (progn
                 (insert "Scratch buffer for " (symbol-name mode) "\n\n")
                 (point))))
            (delete-backward-char (length temp-content)))))
      (setq osf-scratch-buffer? t)
      (run-hooks osf-scratch-create-buffer-hook))
    (pop-to-buffer buffer)))

(provide 'osf-scratch)
