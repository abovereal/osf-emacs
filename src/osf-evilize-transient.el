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

(defun osf--transient-search-forward ()
  (interactive)
  ;; Just disable overriding key bindings, don't enter isearch-mode,
  ;; just use regular evil search.
  (transient--isearch-setup)
  (osf--evilize-transient-search-setup)
  (call-interactively
   (if (eq evil-search-module 'evil-search)
       #'evil-ex-search-forward
     #'evil-search-forward)))

(defun osf--transient-search-backward ()
  (interactive)
  (transient--isearch-setup)
  (osf--evilize-transient-search-setup)
  (call-interactively
   (if (eq evil-search-module 'evil-search)
       #'evil-ex-search-backward
     #'evil-search-backward)))

(dolist (map (list transient-map
                   transient-edit-map))
  (osf-keymap-set map
    ;; Make sure `osf--transient-search-backward' can bind to "?".
    "?" :remove
    "C-?" #'transient-help))

(osf-keymap-set transient-predicate-map
  "<osf--transient-search-forward>" #'transient--do-move
  "<osf--transient-search-backward>" #'transient--do-move)

(defvar-keymap osf--evilize-transient-search-mode-map
  "C-g" #'osf--evilize-ori-transient-search-exit
  "C-q" #'osf--evilize-ori-transient-search-exit)

(define-minor-mode osf--evilize-transient-search-mode
  "The mode enabled after our evil transient search start,
Bind C-g and C-q to exit the search.")

(defun osf--evilize-transient-search-setup ()
  (interactive)
  (osf--evilize-transient-search-mode 1))

(defun osf--evilize-ori-transient-search-exit ()
  (interactive)
  (osf--evilize-transient-search-mode -1)
  (when (eq evil-search-module 'evil-search)
    ;; Don't need to clear highlights if `evil-search-module' is
    ;; 'isearch since `isearch-exit' will do that.
    (evil-ex-nohighlight))
  (transient--isearch-exit))

(osf-keymap-set transient-popup-navigation-map
  "/" #'osf--transient-search-forward
  "?" #'osf--transient-search-backward)

(provide 'osf-evilize-transient)
