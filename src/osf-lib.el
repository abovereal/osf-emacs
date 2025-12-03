;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2025 Zhexuan Chen <2915234902@qq.com>

;; Author: Zhexuan Chen <2915234902@qq.com>
;; URL: https://github.com/CloseToZero/osf-emacs

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

(straight-use-package 'compat)
(require 'compat)

(straight-use-package 'el-patch)
(require 'el-patch)

(straight-use-package 'posframe)

(straight-use-package 'string-inflection)
(require 'string-inflection)

(defun osf-nthcdr (list n)
  (when (>= n 0)
    (cl-loop repeat n
             for cons on list by #'cdr
             finally (return cons))))

(defun osf-truncate-list! (list n)
  "Truncate the LIST to the max length of N, return the truncated list.

Example:
(osf-truncate-list! '(1 2 3) 4) => '(1 2 3)
(osf-truncate-list! '(1 2 3 4) 4) => '(1 2 3 4)
(osf-truncate-list! '(1 2 3 4 5) 4) => '(1 2 3 4)"
  (cond ((zerop n) nil)
        (t (when-let* ((cons (osf-nthcdr list (- n 1))))
             (setcdr cons nil))
           list)))

(defun osf-font-exists? (font)
  (when (fboundp #'x-list-fonts)
    (not (null (x-list-fonts font)))))

(defun osf-edit-config ()
  (interactive)
  (find-file user-init-file))

(cl-defun osf-copy-current-path (&key copy-empty-string (show-message t))
  (interactive)
  (let ((path (expand-file-name default-directory)))
    (when-let* ((file-name (buffer-file-name)))
      (setq path (expand-file-name file-name path)))
    (when (or (not (string-empty-p path)) copy-empty-string)
      (if (eq last-command 'kill-region)
          (kill-append path nil)
        (kill-new path))
      (when show-message
        (message "%s" path)))))

(defvar osf-create-src-hist nil)
(osf-add-saved-vars 'osf-create-src-hist)
(defun osf-create-src (basename)
  (interactive
   (list (read-string "Name: " nil 'osf-create-src-hist)))
  (let* ((feature (concat "osf-" basename))
	 (filepath (expand-file-name (concat feature ".el") osf-src-dir)))
    (when (file-exists-p filepath)
      (user-error "File ~A already exists" filepath))
    (find-file filepath)
    (setq-local lexical-binding t)
    (insert ";;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2025 Zhexuan Chen <2915234902@qq.com>

;; Author: Zhexuan Chen <2915234902@qq.com>
;; URL: https://github.com/CloseToZero/osf-emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs \"28.1\"))

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

")
    (save-excursion
      (insert "

(provide '" feature ")"))
    (save-buffer)))

(defun osf--ad-inhibit-message (fn &rest args)
  (let ((inhibit-message (not (called-interactively-p 'interactive))))
    (apply fn args)))

(defun osf-inhibit-message (fn)
  (advice-add fn :around #'osf--ad-inhibit-message))

(defun osf-keymap-set (keymap &rest bindings)
  "Like `keymap-set', but allow define multiple bindings together.
Example:
(osf-keymap-set keymap1
  \"a\" #'command1
  \"b\" #'command2)"
  (declare (indent defun))
  (cl-loop for (key def) on bindings by #'cddr
           do
           (if (eq def :remove)
               (keymap-unset keymap key t)
             (keymap-set keymap key def))))

(defun osf-keymap-global-set (&rest bindings)
  "Like `keymap-global-set', but allow define multiple bindings together.
Example:
(osf-keymap-global-set
  \"a\" #'command1
  \"b\" #'command2)"
  (declare (indent defun))
  (cl-loop for (key def) on bindings by #'cddr
           do (keymap-global-set key def)))

(defun osf-open-by-system-default-app (file)
  (interactive "fOpen: ")
  (if (eq osf-system-type 'windows)
      (w32-shell-execute "open" (expand-file-name file))
    (call-process (pcase osf-system-type
                    ('mac "open")
                    ((or 'linux 'bsd) "xdg-open"))
                  nil 0 nil (expand-file-name file))))

(defun osf-ff-find-other-file-ignore-include-line
    (&optional in-other-window event)
  (interactive (list current-prefix-arg last-nonmenu-event))
  (ff-find-other-file in-other-window event))

(defun osf-random-alphanum ()
  (let* ((alphanum "abcdefghijklmnopqrstuvwxyz0123456789")
         (i (% (abs (random)) (length alphanum))))
    (elt alphanum i)))

(defun osf-random-text (n)
  (cl-loop with text = (make-string n 0)
           for i from 0 below n
           do (aset text i (osf-random-alphanum))
           finally (return text)))

(defun osf-insert-random-text (n)
  "Insert n characters random text."
  (interactive "nN: ")
  (insert (osf-random-text n)))

(defun osf-message-without-logging (format-string &rest args)
  (let ((message-log-max nil))
    (funcall #'message format-string args)))

(defmacro osf-annotate-within-function (fn &optional var-sym)
  "Advice the function FN to tell others we are within the function FN
by the variable SYM.
This function will define the variable VAR-SYM
by `defvar' and use it to inform others let we are within the function
FN.
By default, VAR-SYM will named osf-within-FN? (NOTE: the FN need to
be a symbol.)"
  (let ((var-sym
         (or var-sym (intern (format "osf-within-%s?" (symbol-name fn)))))
        (advice-sym
         (intern (format "osf--annotate-within-function-%s" (symbol-name fn)))))
    `(progn
       (defvar ,var-sym nil)
       (defun ,advice-sym (fn &rest args)
         (let ((,var-sym t))
           (apply fn args)))
       (advice-add ',fn :around ',advice-sym))))

(defun osf-indexed-reset-prefix-arg ()
  (interactive)
  ;; We don't need to set the `current-prefix-arg' to nil,
  ;; the prefix is already consumed by this command.
  (minibuffer-message "Prefix reset"))

(defun osf--indexed-print-selected-candidate ()
  ;; It's not clear from the documentation what's the relationship
  ;; between `prefix-arg' and `current-prefix-arg', but I found
  ;; `current-prefix-arg' may not up-to-date after invoked
  ;; `digit-argument', `negative-argument' etc, and sometimes
  ;; `prefix-arg' is nil but `current-prefix-arg' is non-nil or vice
  ;; verse, prefer `prefix-arg' and use `current-prefix-arg' as
  ;; fallback seems to give the correct up-to-date prefix.
  (if (and (null prefix-arg) (null current-prefix-arg))
      (minibuffer-message "Prefix arg not used")
    (minibuffer-message
     "Select %sth candidate"
     (prefix-numeric-value (or prefix-arg current-prefix-arg)))))

(defun osf-indexed-show-prefix-arg (arg)
  (interactive "P")
  (osf--indexed-print-selected-candidate)
  ;; Keep the consumed prefix arg.
  (setq prefix-arg arg))

(defun osf-indexed-digit-argument (arg)
  (interactive "P")
  (digit-argument arg)
  (osf--indexed-print-selected-candidate))

;; It's meaningless to select the negative-th candidate, we keep the
;; following command just for consistency (e.g. print prefix arg).
(defun osf-indexed-negative-argument (arg)
  (interactive "P")
  (negative-argument arg)
  (osf--indexed-print-selected-candidate))

(defun osf-indexed-setup-keymap (keymap)
  (osf-keymap-set keymap
    "M-=" #'osf-indexed-show-prefix-arg
    "M-DEL" #'osf-indexed-reset-prefix-arg
    "M-1" #'osf-indexed-digit-argument
    "M-2" #'osf-indexed-digit-argument
    "M-3" #'osf-indexed-digit-argument
    "M-4" #'osf-indexed-digit-argument
    "M-5" #'osf-indexed-digit-argument
    "M-6" #'osf-indexed-digit-argument
    "M-7" #'osf-indexed-digit-argument
    "M-8" #'osf-indexed-digit-argument
    "M-9" #'osf-indexed-digit-argument
    "M-0" #'osf-indexed-digit-argument
    "M--" #'osf-indexed-negative-argument))

(defun osf-hide-cursor ()
  (setq-local cursor-type nil
              evil-emacs-state-cursor '(nil)
              evil-motion-state-cursor '(nil)
              evil-normal-state-cursor '(nil)
              evil-visual-state-cursor '(nil)
              evil-insert-state-cursor '(nil)))

(defun osf-isearch-failed? ()
  (or (not isearch-success) isearch-error))

(defun osf-native-path (path)
  (pcase osf-system-type
    ('windows
     (string-replace "/" "\\" path))
    (_
     path)))

(defvar osf-kill-ring-long-contents-length 100000)
(defun osf-remove-long-contents-in-kill-ring (set-value-directly)
  (interactive (list t))
  (let ((new-kill-ring (cl-remove-if
                        (lambda (v)
                          (> (length v) osf-kill-ring-long-contents-length))
                        kill-ring)))
    (when set-value-directly
      (setq kill-ring new-kill-ring))
    new-kill-ring))

(defun osf-kill-invisible-dired-buffers ()
  (interactive)
  (let* ((invisible-dired-buffers
         (seq-filter
          (lambda (buf)
            (with-current-buffer buf
              (and (eq major-mode 'dired-mode)
                   (not (get-buffer-window buf 'visible)))))
          (buffer-list)))
        (killed-paths
         (mapcar
          (lambda (buf)
            (with-current-buffer buf
              (if (consp dired-directory)
                  (car dired-directory)
                dired-directory)))
          invisible-dired-buffers))
        (killed-buffer-names (mapcar #'buffer-name invisible-dired-buffers)))
      (dolist (buf invisible-dired-buffers)
        (kill-buffer buf))
      (if killed-buffer-names
          (let ((result-buffer (get-buffer-create "*Killed Dired Buffers*")))
            (with-current-buffer result-buffer
              (erase-buffer)
              (insert "Killed dired buffers:\n\n")
              (dolist (path killed-paths)
                (insert path "\n"))
              (goto-char (point-min))
              (view-mode 1))
            (pop-to-buffer result-buffer))
        (message "There are not invisible dired Buffers"))))

(provide 'osf-lib)
