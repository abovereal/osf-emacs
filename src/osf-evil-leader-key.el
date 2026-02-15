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

(defvar osf-leader-key "SPC"
  "Leader key (take effect in `osf-leader-key-states').")

(defvar osf-aux-leader-key "M-SPC"
  "Auxiliary leader key (take effect in `osf-aux-leader-key-states').")

(defvar osf-leader-key-states '(motion normal visual))

(defvar osf-aux-leader-key-states '(insert))

(defvar osf-local-leader-key (concat osf-leader-key " m")
  "Local leader key (take effect in `osf-local-leader-key-states').")

(defvar osf-aux-local-leader-key (concat osf-aux-leader-key " m")
  "Auxiliary local leader key (take effect in
`osf-aux-local-leader-key-states').")

(defvar osf-local-leader-key-states osf-leader-key-states)

(defvar osf-aux-local-leader-key-states osf-aux-leader-key-states)

(defun osf--leader-define-key (keymap local &rest bindings)
  (when bindings
    (let* ((leader-key (if local osf-local-leader-key osf-leader-key))
           (aux-leader-key
            (if local osf-aux-local-leader-key osf-aux-leader-key))
           (leader-key-bindings
            (cl-loop for (key def) on bindings by #'cddr
                     collect (concat leader-key " " key)
                     and collect def))
           (aux-leader-key-bindings
            (cl-loop for (key def) on bindings by #'cddr
                     collect (concat aux-leader-key " " key)
                     and collect def)))
      (apply #'osf-evil-define-key
             (if local osf-local-leader-key-states
               osf-leader-key-states)
             keymap leader-key-bindings)
      (apply #'osf-evil-define-key
             (if local osf-aux-local-leader-key-states
               osf-aux-leader-key-states)
             keymap aux-leader-key-bindings))))

(defun osf-leader-define-key (keymap &rest bindings)
  "Define leader key bindings."
  (declare (indent defun))
  (apply #'osf--leader-define-key keymap nil bindings))

(defun osf-local-leader-define-key (keymap &rest bindings)
  "Define local leader key bindings."
  (declare (indent defun))
  (apply #'osf--leader-define-key keymap t bindings))

(defun osf-release-leader-key (keymaps)
  (mapc (lambda (keymap)
          (osf-evil-define-key osf-leader-key-states
            keymap osf-leader-key nil)
          (osf-evil-define-key osf-aux-leader-key-states
            keymap osf-aux-leader-key nil)
          (when (keymapp keymap)
            (osf-keymap-set keymap osf-leader-key nil)
            (osf-keymap-set keymap osf-aux-leader-key nil)))
        (if (and (listp keymaps) (not (keymapp keymaps)))
            keymaps
          (list keymaps))))

(osf-release-leader-key 'global)
(with-eval-after-load 'magit
  (osf-release-leader-key (list magit-mode-map
                                magit-status-mode-map
                                magit-diff-mode-map
                                magit-blame-read-only-mode-map)))
(with-eval-after-load 'info
  (osf-release-leader-key Info-mode-map))
(with-eval-after-load 'dired
  (osf-release-leader-key dired-mode-map))

(provide 'osf-evil-leader-key)
