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

(setq comint-prompt-read-only t)

;; Add the following command to deal with the case that the eshell
;; buffer has fired the error 'Text is read-only', so I can't use the
;; 'clear' eshell command to clear the buffer and simply can't do
;; anything to fix the eshell buffer.
(defun osf-eshell-clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(with-eval-after-load 'esh-mode
  (osf-evil-define-key '(normal insert) eshell-mode-map
    "C-j" #'eshell-next-prompt
    "C-k" #'eshell-previous-prompt
    "RET" #'eshell-send-input)

  (evil-declare-motion 'eshell-previous-prompt)
  (evil-declare-motion 'eshell-next-prompt)

  ;; Taken from Doom Emacs
  (evil-define-operator osf-eshell-evil-delete (beg end type register yank-handler)
    "Like `evil-delete' but will not delete/copy the prompt."
    (interactive "<R><x><y>")
    (save-restriction
      (narrow-to-region eshell-last-output-end (point-max))
      (evil-delete (if beg (max beg (point-min)) (point-min))
                   (if (eq type 'line) (point-max) (min (or end (point-max)) (point-max)))
                   type register yank-handler)))

  ;; Taken from Doom Emacs
  (evil-define-operator osf-eshell-evil-delete-line (beg end type register yank-handler)
    "Change to end of line."
    :motion nil
    :keep-visual t
    (interactive "<R><x>")
    (osf-eshell-evil-delete (point) end type register yank-handler))

  (osf-evil-define-key 'normal eshell-mode-map
    "d" #'osf-eshell-evil-delete
    "D" #'osf-eshell-evil-delete-line
    "^" #'eshell-bol))

(straight-use-package 'eshell-z)

(with-eval-after-load 'eshell
  (require 'eshell-z))

(defvar osf--async-shell-buffers-regexp
  (rx (* any) "*Async Shell Command" (* any)))

(defun osf--setup-async-shell-buffer ()
  "Setup the async shell buffer: bind q to `quit-window'."
  (when (string-match osf--async-shell-buffers-regexp (buffer-name))
    (osf-evil-define-key 'normal 'local
      "q" #'quit-window)))

(add-hook 'shell-mode-hook #'osf--setup-async-shell-buffer)

;; Start async shell buffers in normal state
(add-to-list 'evil-buffer-regexps (cons osf--async-shell-buffers-regexp 'normal))

(defun osf-async-shell-command (command &optional output-buffer error-buffer)
  "Like `async-shell-command', but will select the shell output buffer."
  (interactive
   (list
    (read-shell-command
     (if shell-command-prompt-show-cwd
         (format-message "Async shell command in `%s': "
                         (abbreviate-file-name
                          default-directory))
       "Async shell command: ")
     nil nil
     (let ((filename
            (cond
             (buffer-file-name)
             ((eq major-mode 'dired-mode)
              (dired-get-filename nil t)))))
       (and filename (file-relative-name filename))))
    nil
    shell-command-default-error-buffer))
  (async-shell-command command output-buffer error-buffer)
  ;; Why I can't get buffer directly?
  (when-let* ((buffer (get-buffer (or output-buffer shell-command-buffer-name-async))))
    (pop-to-buffer buffer)))

(provide 'osf-shell)
