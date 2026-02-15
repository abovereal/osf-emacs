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

(defvar osf-tc-executable
  nil
  "The executable of Total Commander.")

(defun osf-tc-executable ()
  (unless osf-tc-executable
    (setq osf-tc-executable
          (or (executable-find "TOTALCMD64")
              (when-let* ((path (getenv "COMMANDER_PATH")))
                (expand-file-name "TOTALCMD64.exe" path))))
    (unless (file-executable-p osf-tc-executable)
      (setq osf-tc-executable nil))
    (unless osf-tc-executable
      (error "Cannot find the executable of Total Commander")))
  osf-tc-executable)

(defun osf-tc-open-dir (dir)
  (interactive "DDir: ")
  (call-process (osf-tc-executable) nil 0 nil "/S" "/O" "/T" (expand-file-name dir)))

(osf-leader-define-key 'global
  "f t" #'osf-tc-open-dir)

(provide 'osf-total-commander)
