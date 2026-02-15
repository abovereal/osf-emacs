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

(straight-use-package 'magit)

(with-eval-after-load 'project
  (when (boundp 'project-prefix-map)
    (keymap-set project-prefix-map "m" #'magit-project-status)
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)))

(defun osf--git-commit-start-in-insert-state-if-blank ()
  "Start `git-commit-mode' in insert state if in a blank commit message,
otherwise in default state."
  (when (and (bound-and-true-p evil-mode)
             (not (evil-emacs-state-p))
             (bobp) (eolp))
    (evil-insert-state)))
(add-hook 'git-commit-setup-hook
          #'osf--git-commit-start-in-insert-state-if-blank)

(osf-leader-define-key 'global
  "g g" #'magit-status)

(with-eval-after-load 'magit
  (transient-define-suffix magit-submodule-update-all (args)
    "Update all submodules"
    :class 'magit--git-submodule-suffix
    :description "Update all modules recursively"
    (interactive (list (magit-submodule-arguments "--recursive")))
    (magit-with-toplevel
      (magit-run-git-async "submodule" "update" "--init" args)))

  (transient-append-suffix 'magit-submodule '(2 -1)
    '("U" magit-submodule-update-all)))

(provide 'osf-vc)
