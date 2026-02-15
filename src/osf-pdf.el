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

(straight-use-package 'pdf-tools)

(add-hook 'pdf-view-mode-hook #'osf-hide-cursor)

(pdf-loader-install)

(defun osf--dont-ask-if-pdf-file-too-large (fn size op-type filename &optional offer-raw)
  (cond ((string-match-p (rx "." (regexp (osf-regexp-literal-any-case "pdf")) string-end) filename)
         (let ((large-file-warning-threshold nil))
           (funcall fn size op-type filename offer-raw)))
        (t (funcall fn size op-type filename offer-raw))))

(advice-add #'abort-if-file-too-large :around #'osf--dont-ask-if-pdf-file-too-large)

(defun osf-pdf-view-kill-outline-buffer (&optional pdf-buffer)
  "Kill the outline buffer of PDF-BUFFER (defaulted to the current buffer)."
  (when-let* ((buffer-name (pdf-outline-buffer-name pdf-buffer))
             (buffer (get-buffer buffer-name)))
    (kill-buffer buffer)))

(defun osf--pdf-view-setup-kill-outline-buffer-hook ()
    (add-hook 'kill-buffer-hook #'osf-pdf-view-kill-outline-buffer nil t))
(add-hook 'pdf-view-mode-hook #'osf--pdf-view-setup-kill-outline-buffer-hook)

(straight-use-package 'saveplace-pdf-view)

;; Need enable save-place-mode in the other place.
(with-eval-after-load 'saveplace
  (with-eval-after-load 'pdf-tools
    (require 'saveplace-pdf-view)))

(with-eval-after-load 'saveplace-pdf-view
  (defun osf--saveplace-pdf-view-dont-save-if-no-built (fn &rest args)
    "Don't save the place of pdf-view buffers if pdf-tools haven't been installed.
Otherwise, we will lose the save-place history of pdf-view buffers."
    (cond ((or (derived-mode-p 'pdf-view-mode)
               (and (buffer-file-name)
                    (string-match-p
                     (rx "." (regexp (osf-regexp-literal-any-case "pdf")) string-end)
                     (buffer-file-name))))
           (when (ignore-errors (pdf-info-check-epdfinfo) t)
             (apply fn args)))
          (t
           (apply fn args))))
  (advice-add #'saveplace-pdf-view-to-alist-advice :around
              #'osf--saveplace-pdf-view-dont-save-if-no-built)

  (defun osf--saveplace-pdf-view-restore-history-after-rebuilt
      (fn
       target-directory
       &optional
       skip-dependencies-p
       force-dependencies-p
       callback
       build-directory)
    "Restore pdf-view buffers' save-place history after pdf-tools installed."
    (apply
     fn
     target-directory
     skip-dependencies-p
     force-dependencies-p
     (lambda (executable &rest args)
       (prog1 (and callback (apply callback executable args))
         (when executable
           (dolist (buf (buffer-list))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when (derived-mode-p 'pdf-view-mode)
                   ;; Switch to buf's window, avoid `pdf-view-bookmark-jump'
                   ;; switch the buffer of current window to buf.
                   (with-selected-window (or (get-buffer-window buf) (selected-window))
                     (save-place-find-file-hook)))))))))
     build-directory))
  (advice-add #'pdf-tools-build-server
              :around #'osf--saveplace-pdf-view-restore-history-after-rebuilt))

(defvar osf-pdf-view-registers nil)
(defvar osf-pdf-view-delete-register-history nil)
(osf-add-saved-vars 'osf-pdf-view-registers 'osf-pdf-view-delete-register-history)

(with-eval-after-load 'pdf-tools
  (defvar osf-pdf-view-registers-max-files 100)
  (defvar osf-pdf-view-max-registers-per-file 20)

  (defun osf-serialize-pdf-view-register-alist (register-alist)
    (mapcar (lambda (register)
              (let ((register-struct (cdr register)))
                (cons (car register)
                      (registerv-data register-struct))))
            register-alist))

  (defun osf-deserialize-pdf-view-register-alist (register-alist)
    (mapcar (lambda (register)
              (let ((register-data (cdr register)))
                (cons (car register)
                      (registerv-make
                       register-data
                       :print-func 'pdf-view-registerv-print-func
                       :jump-func 'pdf-view-bookmark-jump
                       :insert-func (lambda (bmk)
                                      (insert (format "%S" bmk)))))))
            register-alist))

  (defun osf-pdf-view-save-registers (&rest _)
    "Save/Remember the registers of each pdf-view-mode buffers.
Using `buffer-file-name' as keys."
    (setf (alist-get (buffer-file-name) osf-pdf-view-registers nil nil #'string=)
          (osf-serialize-pdf-view-register-alist
           ;; Truncation for each file.
           (if (> (length pdf-view-register-alist) osf-pdf-view-max-registers-per-file)
               (osf-truncate-list! osf-pdf-view-max-registers-per-file pdf-view-register-alist)
             pdf-view-register-alist)))
    ;; Truncation for each files.
    (when (> (length osf-pdf-view-registers) osf-pdf-view-registers-max-files)
      (osf-truncate-list! osf-pdf-view-registers-max-files osf-pdf-view-registers)))
  (advice-add #'pdf-view-position-to-register :after #'osf-pdf-view-save-registers)

  (defun osf--pdf-view-restore-saved-registers ()
    (when-let* ((registers (alist-get (buffer-file-name)
                                      osf-pdf-view-registers nil nil #'string=)))
      (setq pdf-view-register-alist (osf-deserialize-pdf-view-register-alist registers))))
  (add-hook 'pdf-view-mode-hook #'osf--pdf-view-restore-saved-registers)

  (defun osf-pdf-view-delete-register (register-name)
    (interactive (list
                  (let ((active-register-names
                         (mapcar (lambda (x) (string (car x))) pdf-view-register-alist)))
                    (completing-read "Delete register: "
                                     active-register-names nil t nil
                                     'osf-pdf-view-delete-register-history
                                     active-register-names))))
    (setf (alist-get (string-to-char register-name) pdf-view-register-alist nil 'remove) nil)
    (osf-pdf-view-save-registers))

  (defun osf-pdf-view-delete-all-registers ()
    (interactive)
    (setq pdf-view-register-alist nil)
    (osf-pdf-view-save-registers))

  (osf-evil-define-key 'normal pdf-view-mode-map
    "r" nil
    "r d" #'osf-pdf-view-delete-register
    "r D" #'osf-pdf-view-delete-all-registers))

(provide 'osf-pdf)
