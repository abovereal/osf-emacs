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

(set-language-environment "UTF-8")
(when (boundp 'w32-ansi-code-page)
  (defvar osf-w32-locale-coding-system
    (intern (format "cp%d" w32-ansi-code-page)))
  (prefer-coding-system osf-w32-locale-coding-system)
  (setq file-name-coding-system osf-w32-locale-coding-system)
  (prefer-coding-system 'utf-8)

  (defun osf-process-regexp-for-program (program-name)
    "Return a regexp for PROGRAM-NAME to be used in `process-coding-system-alist'.
The regexp will match PROGRAM-NAME with optional .exe suffix in both
name only form or path form.  NOTE: The PROGRAM-NAME is matched in
case insensitive way.
Example:
Assume PROGRAM-NAME is \"rg\", the regexp will match any of the following:
rg
rg.exe
path\\to\\rg
path\\to\\rg.exe
path\\to\\Rg.exe
path\\to\\rG.exe
path/to/rg
path/to/rg.exe
but will not match any of the following:
rg.e
path/to/arg.exe
path\\to\\arg.exe"
    (rx (| (seq (or "/" "\\")
                (regexp (osf-regexp-literal-any-case program-name))
                (? ".exe")
                string-end)
           (seq string-start
                (regexp (osf-regexp-literal-any-case program-name))
                (? ".exe")
                string-end))))
  (dolist (program '("rg" "magick" "dot" "TOTALCMD64" "cmder" "git"))
    (modify-coding-system-alist
     'process (osf-process-regexp-for-program program)
     (cons 'utf-8 osf-w32-locale-coding-system)))
  (with-eval-after-load 'magit
    ;; We already change the encoding between the git executable and Emacs,
    ;; so magit should not encode args anymore.
    (defun osf//magit-process-git-arguments-dont-encode-args (args)
      (setq args (append magit-git-global-arguments (flatten-tree args))))
    (advice-add #'magit-process-git-arguments
                :override #'osf//magit-process-git-arguments-dont-encode-args))
  (modify-coding-system-alist
   'process (osf-process-regexp-for-program "cmdproxy")
   (cons osf-w32-locale-coding-system osf-w32-locale-coding-system)))

(provide 'osf-coding-system)
