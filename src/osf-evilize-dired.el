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

(apply
 #'osf-evil-define-key 'normal dired-mode-map
 `("q" quit-window
   "e" evil-forward-word-end
   "B" evil-backward-WORD-begin
   "j" evil-next-visual-line
   "k" evil-previous-visual-line
   "C-j" dired-next-dirline
   "C-k" dired-prev-dirline
   "g r" revert-buffer
   "+" dired-create-directory
   "=" dired-diff
   "~" dired-flag-backup-files
   "." dired-clean-directory
   "d" dired-flag-file-deletion
   "x" dired-do-flagged-delete
   "m" dired-mark
   "u" dired-unmark
   "U" dired-unmark-all-marks
   "<delete>" dired-unmark-backward
   "t" dired-toggle-marks
   "RET" dired-find-file
   "S-<return>" dired-find-file-other-window
   "M-<return>" dired-display-file
   "^" dired-up-directory
   "%" nil
   "% u" dired-upcase
   "% l" dired-downcase
   "% r" dired-flag-files-regexp
   "% m" dired-mark-files-regexp
   "% M" dired-mark-files-containing-regexp
   "% C" dired-do-copy-regexp
   "% R" dired-do-rename-regexp
   "% H" dired-do-hardlink-regexp
   "% S" dired-do-symlink-regexp
   "% ." dired-flag-garbage-files
   "*" nil
   "* e" dired-mark-executables
   "* d" dired-mark-directories
   "* s" dired-mark-symlinks
   "* S" dired-mark-subdir-files
   "* r" dired-mark-files-regexp
   "* c" dired-change-marks
   "]" dired-next-marked-file
   "[" dired-prev-marked-file
   "C" dired-do-copy
   "R" dired-do-rename
   "B" dired-do-byte-compile
   "!" dired-do-shell-command
   "&" dired-do-async-shell-command
   "D" dired-do-delete
   "H" dired-up-directory
   "L" dired-do-load
   "c M" dired-do-chmod
   "c O" dired-do-chown
   "c G" dired-do-chgrp
   "Q" dired-do-find-regexp-and-replace
   "Z Z" dired-do-compress
   "Z T" dired-do-compress-to
   "K" nil
   "K K" dired-do-kill-lines
   "I" dired-maybe-insert-subdir
   "K D" dired-kill-subdir
   "-" dired-undo
   "g j" dired-next-subdir
   "g k" dired-prev-subdir
   "g u" dired-tree-up
   "g d" dired-tree-down
   "$" evil-end-of-line
   "^" evil-first-non-blank
   "TAB" dired-hide-subdir
   "<backtab>" dired-hide-all
   "g g" evil-goto-first-line
   "G" evil-goto-line
   "v" evil-visual-char
   "y f" dired-copy-filename-as-kill
   "y p" osf-dired-copy-path
   "y c" osf-copy-current-path
   "Y" evil-yank-line
   "?" evil-ex-search-forward
   ,@(cond ((eq evil-search-module 'evil-search)
            '("/" evil-ex-search-forward
              "?" evil-ex-search-backward
              "n" evil-ex-search-next
              "N" evil-ex-search-previous))
           (t
            '("/" evil-search-forward
              "?" evil-search-backward
              "n" evil-search-next
              "N" evil-search-previous)))
   "i" wdired-change-to-wdired-mode
   ,@(when (fboundp #'image-dired-show-all-from-dir)
       '("M ." image-dired-display-thumb
         "M A" image-dired-show-all-from-dir
         "M a" image-dired-display-thumbs-append
         "M c" image-dired-dired-comment-files
         "M d" image-dired-display-thumbs
         "M e" image-dired-dired-edit-comment-and-tags
         "M f" image-dired-mark-tagged-files
         "M i" image-dired-dired-display-image
         "M j" image-dired-jump-thumbnail-buffer
         "M r" image-dired-delete-tag
         "M t" image-dired-tag-files
         "M x" image-dired-dired-display-external))))

(evil-set-initial-state 'dired-mode 'normal)

(provide 'osf-evilize-dired)
