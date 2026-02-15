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

(evil-set-initial-state 'help-mode 'normal)

(apply
 #'osf-evil-define-key 'normal help-mode-map
 `("q" quit-window
   "g r" revert-buffer
   "<tab>" forward-button
   "<backtab>" backward-button
   "RET" push-button
   "C-o" help-go-back
   "C-i" help-go-forward
   "K" help-follow-symbol
   ,@(when (>= emacs-major-version 28)
       '("s" help-view-source
         "i" help-goto-info
         "c" help-customize))))

(provide 'osf-evilize-help)
