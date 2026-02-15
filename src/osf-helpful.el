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

(straight-use-package 'helpful)

(osf-keymap-set help-map
  "f" #'helpful-callable
  "F" #'helpful-function
  "v" #'helpful-variable
  "k" #'helpful-key
  "x" #'helpful-command
  "." #'helpful-at-point)

(with-eval-after-load 'helpful
  (evil-set-initial-state 'helpful-mode 'normal)

  (osf-evil-define-key 'normal helpful-mode-map
    "q" #'quit-window
    "g r" #'helpful-update
    "g h" #'describe-mode
    "<tab>" #'forward-button
    "<backtab>" #'backward-button
    "RET" #'helpful-visit-reference))

(provide 'osf-helpful)
