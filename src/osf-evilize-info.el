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

(evil-set-initial-state 'Info-mode 'normal)

(osf-evil-define-key 'normal Info-mode-map
  "<tab>" #'Info-next-reference
  "S-<tab>" #'Info-prev-reference
  "RET" #'Info-follow-nearest-node
  "a" #'info-apropos
  "C-o" #'Info-history-back
  "C-i" #'Info-history-forward
  "C-j" #'Info-forward-node
  "C-k" #'Info-backward-node
  "g d" #'Info-directory
  "g i" #'Info-index
  "g I" #'Info-virtual-index
  "g j" #'Info-next
  "g k" #'Info-prev
  "g m" #'Info-menu
  "g t" #'Info-top-node
  "g T" #'Info-toc
  "g u" #'Info-up
  "g f" #'Info-follow-reference)

(provide 'osf-evilize-info)
