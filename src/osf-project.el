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

;; Adapt from `projectile-save-project-buffers'.
(defun osf-project-save-buffers ()
  "Save all project buffers."
  (interactive)
  (let ((project (project-current)))
    (unless project (user-error "Not in a project"))
    (let ((modified-buffers
           (cl-remove-if-not
            #'(lambda (buffer)
                (and (buffer-file-name buffer)
                     (buffer-modified-p buffer)))
            (project-buffers project))))
      (cond ((null modified-buffers) (message "No buffers need saving"))
            (t (dolist (buffer modified-buffers)
                 (with-current-buffer buffer (save-buffer)))
               (message "Saved %d buffers" (length modified-buffers)))))))

(osf-keymap-set project-prefix-map
  "S" #'osf-project-save-buffers)

(osf-leader-define-key 'global
  "p" project-prefix-map)

(setq project-vc-merge-submodules nil)

(provide 'osf-project)
