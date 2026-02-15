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

(evil-set-initial-state 'imenu-list-major-mode 'normal)

(osf-evil-define-key 'normal imenu-list-major-mode-map
  "RET" #'imenu-list-ret-dwim
  "M-<return>" #'imenu-list-display-dwim
  "j" #'evil-next-visual-line
  "k" #'evil-previous-visual-line
  "<tab>" #'hs-toggle-hiding
  "f" #'hs-toggle-hiding
  "g r" #'imenu-list-refresh
  "q" #'imenu-list-quit-window)

(provide 'osf-evilize-imenu-list)
