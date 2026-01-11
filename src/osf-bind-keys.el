;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2025 Zhexuan Chen <2915234902@qq.com>

;; Author: Zhexuan Chen <2915234902@qq.com>
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

;; Unbind `view-hello-file', it hangs Emacs.
(osf-keymap-set help-map "h" nil)

(osf-leader-define-key 'global
  "SPC" #'execute-extended-command

  "h" help-map

  "b x" #'kill-current-buffer
  "b m" #'ibuffer

  "d d" #'osf-reveal-current-file

  "B s" #'bookmark-set
  "B j" #'bookmark-jump
  "B r" #'bookmark-rename

  "f f" #'find-file
  "f r" #'revert-buffer
  "f c" #'osf-edit-config
  "f o" #'osf-ff-find-other-file-ignore-include-line

  "q q" #'save-buffers-kill-terminal
  "q r" #'restart-emacs

  "% l" #'query-replace
  "% r" #'query-replace-regexp

  "y c" #'osf-copy-current-path)

(osf-keymap-global-set
  "<wheel-left>" #'ignore
  "<wheel-right>" #'ignore

  "M-&" #'osf-async-shell-command)

(provide 'osf-bind-keys)
