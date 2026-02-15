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

(with-eval-after-load 'replace
  (evil-set-initial-state 'occur-mode 'normal)

  (evil-set-initial-state 'occur-edit-mode 'normal)

  (osf-evil-define-key 'normal occur-mode-map
    "q" #'quit-window
    "C-j" #'next-error-no-select
    "C-k" #'previous-error-no-select
    "M-j" #'occur-next
    "M-k" #'occur-prev
    "RET" #'occur-mode-goto-occurrence
    "M-<return>" #'occur-mode-display-occurrence
    "i" #'occur-edit-mode
    "g r" #'revert-buffer
    "r" #'occur-rename-buffer)

  (osf-evil-define-key nil occur-edit-mode-map
    "<remap> <evil-quit>" #'occur-cease-edit
    "<remap> <evil-write>" #'occur-cease-edit)

  (osf-evil-define-key '(normal insert) occur-edit-mode-map
    "M-<return>" #'occur-mode-display-occurrence))

(provide 'osf-occur)
