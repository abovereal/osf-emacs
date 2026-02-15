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

(straight-use-package 'avy)
(defvar avy-all-windows t)
(setq avy-timeout-seconds nil
      avy-single-candidate-jump nil)

(defun osf-avy-goto-char-timer (&optional arg)
  "Like `avy-goto-char-timer', but exit with empty input can resume previous session."
  (interactive "P")
  (require 'avy)
  (let ((avy-all-windows (if arg
                             (not avy-all-windows)
                           avy-all-windows))
        (resume nil))
    (let ((backup-avy--old-cands avy--old-cands)
          (backup-avy-resume (symbol-function 'avy-resume)))
      (avy-with avy-goto-char-timer
        (setq avy--old-cands (avy--read-candidates))
        (cond ((string-empty-p avy-text)
               (setq avy--old-cands backup-avy--old-cands)
               (setf (symbol-function 'avy-resume) backup-avy-resume)
               (setq resume t))
              (t
               (avy-process avy--old-cands))))
      (cond (resume (avy-resume))
            (t (setf (symbol-function 'avy-resume)
                     (lambda ()
                       (interactive)
                       (avy-process avy--old-cands))))))))

(evil-define-avy-motion osf-avy-goto-char-timer inclusive)

(osf-evil-define-key 'normal 'global
  "," #'evil-osf-avy-goto-char-timer)

(provide 'osf-avy)
