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

(with-eval-after-load 'eww
  (let ((maps (list eww-mode-map eww-link-keymap)))
    (dolist (map maps)
      (dolist (binding '(("TAB" nil)
                         ("w" nil)
                         ("v" nil)
                         ("y" nil)))
        (keymap-set map (cl-first binding) (cl-second binding))))
    (dolist (map maps)
      (osf-evil-define-key 'normal map
        "M-h" #'eww-back-url
        "M-l" #'eww-forward-url
        "C-o" #'better-jumper-jump-backward
        "C-i" #'better-jumper-jump-forward
        "<tab>" #'shr-next-link
        "w" #'evil-forward-word-begin
        "y" #'evil-yank
        "v" #'evil-visual-char))
    (osf-evil-define-key 'normal eww-mode-map
      "g r" #'eww-reload)
    (osf-local-leader-define-key eww-mode-map
      "y p" #'eww-copy-page-url
      "y ." #'shr-maybe-probe-and-copy-url)))

(provide 'osf-eww)
