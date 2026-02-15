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


(with-eval-after-load 'ibuffer
  (evil-set-initial-state 'ibuffer-mode 'normal)
  (osf-evil-define-key 'normal ibuffer-mode-map
    "C-d" #'evil-scroll-down

    "d" #'ibuffer-mark-for-delete

    "U" #'ibuffer-unmark-all-marks
    "m" #'ibuffer-mark-forward
    "u" #'ibuffer-unmark-forward

    "RET" #'ibuffer-visit-buffer

    "!" #'ibuffer-do-shell-command-file

    "x" #'ibuffer-do-kill-on-deletion-marks

    "g r" #'ibuffer-update

    "<remap> <evil-insert>" #'ignore

    "s a" #'ibuffer-do-sort-by-alphabetic
    "s f" #'ibuffer-do-sort-by-filename/process
    "s i" #'ibuffer-invert-sorting
    "s m" #'ibuffer-do-sort-by-major-mode
    "s s" #'ibuffer-do-sort-by-size
    "s v" #'ibuffer-do-sort-by-recency

    "| !" #'ibuffer-negate-filter
    "| &" #'ibuffer-and-filter
    "| *" #'ibuffer-filter-by-starred-name
    "| ." #'ibuffer-filter-by-file-extension
    "| /" #'ibuffer-filter-disable
    "| <" #'ibuffer-filter-by-size-lt
    "| <up>" #'ibuffer-pop-filter
    "| >" #'ibuffer-filter-by-size-gt
    "| D" #'ibuffer-decompose-filter-group
    "| E" #'ibuffer-filter-by-process
    "| F" #'ibuffer-filter-by-directory
    "| M" #'ibuffer-filter-by-derived-mode
    "| P" #'ibuffer-pop-filter-group
    "| R" #'ibuffer-switch-to-saved-filter-groups
    "| RET" #'ibuffer-filter-by-mode
    "| S" #'ibuffer-save-filter-groups
    "| S-<up>" #'ibuffer-pop-filter-group
    "| SPC" #'ibuffer-filter-chosen-by-completion
    "| TAB" #'ibuffer-exchange-filters
    "| X" #'ibuffer-delete-saved-filter-groups
    "| \\" #'ibuffer-clear-filter-groups
    "| a" #'ibuffer-add-saved-filters
    "| b" #'ibuffer-filter-by-basename
    "| c" #'ibuffer-filter-by-content
    "| d" #'ibuffer-decompose-filter
    "| e" #'ibuffer-filter-by-predicate
    "| f" #'ibuffer-filter-by-filename
    "| g" #'ibuffer-filters-to-filter-group
    "| i" #'ibuffer-filter-by-modified
    "| m" #'ibuffer-filter-by-used-mode
    "| n" #'ibuffer-filter-by-name
    "| o" #'ibuffer-or-filter
    "| p" #'ibuffer-pop-filter
    "| r" #'ibuffer-switch-to-saved-filters
    "| s" #'ibuffer-save-filters
    "| t" #'ibuffer-exchange-filters
    "| v" #'ibuffer-filter-by-visiting-file
    "| x" #'ibuffer-delete-saved-filters
    "| |" #'ibuffer-or-filter

    "% L" #'ibuffer-mark-by-locked
    "% f" #'ibuffer-mark-by-file-name-regexp
    "% g" #'ibuffer-mark-by-content-regexp
    "% m" #'ibuffer-mark-by-mode-regexp
    "% n" #'ibuffer-mark-by-name-regexp
    "% *" #'ibuffer-unmark-all
    "% /" #'ibuffer-mark-dired-buffers
    "% M" #'ibuffer-mark-by-mode
    "% c" #'ibuffer-change-marks
    "% e" #'ibuffer-mark-dissociated-buffers
    "% h" #'ibuffer-mark-help-buffers
    "% m" #'ibuffer-mark-modified-buffers
    "% r" #'ibuffer-mark-read-only-buffers
    "% s" #'ibuffer-mark-special-buffers
    "% u" #'ibuffer-mark-unsaved-buffers
    "% z" #'ibuffer-mark-compressed-file-buffers
    )

  ;; Disable ibuffer-mode-map as overriding map
  (define-key ibuffer-mode-map [override-state] nil))

(provide 'osf-ibuffer)
