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

(straight-use-package 'blackout)

(pcase-dolist
    (`(,feature ,modes)
     '((autorevert auto-revert-mode)
       (eldoc eldoc-mode)
       (double-trigger double-trigger-mode)
       (company company-mode)
       (simple auto-fill-mode)
       (with-editor with-editor-mode)
       (smartparens smartparens-mode)
       (whitespace whitespace-mode)
       (org-indent org-indent-mode)
       (zig-mode zig-format-on-save-mode)
       (slime-autodoc slime-autodoc-mode)
       (better-jumper better-jumper-local-mode)
       (lispy lispy-mode)
       (lispyville lispyville-mode)
       (evil-snipe evil-snipe-local-mode)
       ;; (feature (mode1 mode2))
       ))
  (with-eval-after-load feature
    (dolist (mode (osf-ensure-is-list modes))
      (blackout mode))))

(setq mode-line-format (delete '(vc-mode vc-mode) mode-line-format))

(provide 'osf-clean-mode-line)
