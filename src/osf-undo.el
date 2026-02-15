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

(straight-use-package 'vundo)

(osf-keymap-global-set
  "C-/" #'undo-only
  "C-?" #'undo-redo)

(with-eval-after-load 'vundo
  (defun osf-vundo-quit ()
    (interactive)
    (when (y-or-n-p
           (substitute-command-keys
            "Quit vundo will roll back the changes,\
 use \\[vundo-confirm] to confirm the changes.\
 Quit? "))
      (let ((vundo-roll-back-on-quit t))
        (vundo-quit)))
    ;; Clear minibuffer message.
    (message nil)))

(provide 'osf-undo)
