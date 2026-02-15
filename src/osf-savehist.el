;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2026 Chen Zhexuan

;; Author: Chen Zhexuan
;; URL: https://github.com/CloseToZero/osf-emacs

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

(defvar savehist-additional-variables nil)

(add-hook 'after-init-hook #'savehist-mode)

(defun osf-add-saved-vars (&rest variables)
  (setq savehist-additional-variables
        (append variables savehist-additional-variables)))

(defun osf--remove-kill-ring-text-properties-for-savehist ()
  (setq-local kill-ring
              (mapcar #'substring-no-properties
                      (cl-remove-if-not #'stringp kill-ring)))
  (osf-remove-long-contents-in-kill-ring t))
(add-hook 'savehist-save-hook #'osf--remove-kill-ring-text-properties-for-savehist)
(osf-add-saved-vars 'kill-ring)

(provide 'osf-savehist)
