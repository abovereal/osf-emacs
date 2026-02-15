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

(osf-evil-define-key 'normal vertico-map
  "]" #'vertico-next-group
  "[" #'vertico-previous-group
  "C-u" #'vertico-scroll-down
  "C-d" #'vertico-scroll-up
  "j" #'vertico-next
  "k" #'vertico-previous
  "g g" #'vertico-first
  "G" #'vertico-last
  "y y" #'vertico-save)

(osf-evil-define-key 'insert vertico-map
  "C-j" #'vertico-next
  "C-k" #'vertico-previous
  "M-<" #'vertico-first
  "M->" #'vertico-last
  "M-w" #'vertico-save)

(osf-evil-define-key '(normal insert) vertico-map
  "RET" #'vertico-exit
  "M-<return>" #'vertico-exit-input)

(provide 'osf-evilize-vertico)
