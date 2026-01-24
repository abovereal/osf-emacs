;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2025 Zhexuan Chen <2915234902@qq.com>

;; Author: Zhexuan Chen <2915234902@qq.com>
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

(setq dired-dwim-target t
      dired-listing-switches "-alh"
      dired-mouse-drag-files t)

(with-eval-after-load 'dired
  (defun osf-dired-copy-path ()
    (interactive)
    (dired-copy-filename-as-kill 0))

  (defun osf-dired-open-by-system-default-app ()
    (interactive)
    (let ((cur-file-name (dired-get-filename t t)))
      (unless cur-file-name
        (user-error "No file on this line"))
      (osf-open-by-system-default-app cur-file-name)))

  (osf-evil-define-key 'normal dired-mode-map
    "M-S-<return>" #'osf-dired-open-by-system-default-app
    "w" #'evil-forward-word-begin)

  (osf-local-leader-define-key dired-mode-map
    "?" #'dired-summary))

(with-eval-after-load 'image-dired-external
  (when (executable-find "magick")
    (setq image-dired-cmd-create-thumbnail-program "magick")
    (unless (member "convert" image-dired-cmd-create-thumbnail-options)
      (setq image-dired-cmd-create-thumbnail-options
            (cons "convert" image-dired-cmd-create-thumbnail-options)))))

(straight-use-package 'dired-hacks-utils)
(with-eval-after-load 'dired
  (require 'dired-hacks-utils)
  (dired-utils-format-information-line-mode))

(add-hook 'dired-mode-hook #'hl-line-mode)

(defun osf-dired-dnd-handle-local-file (uri action)
  (if (yes-or-no-p "open if yes, drop if no ")
      (dnd-open-local-file uri action)
    (dired-dnd-handle-local-file uri action)))

(defun osf-dired-dnd-handle-file (uri action)
  (if (yes-or-no-p "open if yes, drop if no ")
      (dnd-open-local-file uri action)
    (dired-dnd-handle-file uri action)))

(setq dired-dnd-protocol-alist
      '(("^file:///" . osf-dired-dnd-handle-local-file)
        ("^file://"  . osf-dired-dnd-handle-file)
        ("^file:"    . osf-dired-dnd-handle-local-file)))

(provide 'osf-dired)
