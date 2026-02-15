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

(straight-use-package
 '(query-replace-parallel
   :type git
   :host github :repo "hokomo/query-replace-parallel"))
(autoload #'query-replace-parallel "query-replace-parallel")
(autoload #'query-replace-parallel-regexp "query-replace-parallel")

(osf-leader-define-key 'global
  "% p l" #'query-replace-parallel
  "% p r" #'query-replace-parallel-regexp)

(with-eval-after-load 'query-replace-parallel
  (dolist (cmd '(query-replace-parallel
                 query-replace-parallel-regexp))
    (evil-set-command-property cmd :jump t)))

;; Don't terminate the search if we just pressed a control character.
(setq search-exit-option 'edit
      isearch-wrap-pause 'no
      isearch-lazy-count t)

;; Swap the bindings of DEL and C-M-w, so that DEL always delete a
;; character rather than undo the previous search action.
(osf-keymap-set isearch-mode-map
  "DEL" #'isearch-del-char
  "C-M-w" #'isearch-delete-char)

(straight-use-package 'deadgrep)

(autoload #'osf-deadgrep "deadgrep" nil t)

(osf-leader-define-key 'global
  "/ r" #'osf-deadgrep)

(when (executable-find "rg")
  (with-eval-after-load 'project
    (when (boundp 'project-prefix-map)
      (keymap-set project-prefix-map "g" #'deadgrep)
      (setq project-switch-commands
            (cl-delete-if (lambda (e)
                            (eq (car e) 'project-find-regexp))
                          project-switch-commands))
      (add-to-list 'project-switch-commands '(deadgrep "Deadgrep") t))))

(with-eval-after-load 'deadgrep
  (defun osf-deadgrep (search-term &optional directory)
    "Just like `deadgrep', but handle prefix argument in a different way.
When a positive prefix argument is given, create the results buffer but
don’t actually start the search.
When a negative prefix argument is given, start search at `default-directory'
instead of the directory determined by `deadgrep-project-root-function'.
When a prefix argument with numeric value zero is given, the effect is the
combination of positive and negative prefix arguments."
    (interactive
     (list (deadgrep--read-search-term)
           (if (and current-prefix-arg
                    (<= (prefix-numeric-value current-prefix-arg) 0))
               default-directory
             (funcall deadgrep-project-root-function))))
    (if (and current-prefix-arg
             (>= (prefix-numeric-value current-prefix-arg) 0))
        ;; Keep `current-prefix-arg' for `deadgrep'.
        (deadgrep search-term directory)
      (let ((current-prefix-arg nil))
        (deadgrep search-term directory))))

  (defun osf--deadgrep--write-postponed-ad ()
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (insert
         (substitute-command-keys
          "Press \\[deadgrep-restart] to start the search.")))))
  (advice-add #'deadgrep--write-postponed :override #'osf--deadgrep--write-postponed-ad)

  (defun osf-deadgrep-visit-result-other-window ()
    "Like `deadgrep-visit-result-other-window', but stay at the same window."
    (interactive)
    (let ((old-window (selected-window)))
      (deadgrep-visit-result-other-window)
      (select-window old-window)))

  (osf-evil-define-key 'normal deadgrep-mode-map
    "M-<return>" #'osf-deadgrep-visit-result-other-window))

(straight-use-package 'wgrep)
(straight-use-package 'wgrep-deadgrep)
(setq wgrep-auto-save-buffer t)

(with-eval-after-load 'deadgrep
  (osf-evil-define-key 'normal deadgrep-mode-map
    "i" #'wgrep-change-to-wgrep-mode))

(provide 'osf-search-and-replace)
