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

(straight-use-package 'perspective)

(setq persp-suppress-no-prefix-key-warning t
      persp-state-default-file (expand-file-name "perspectives" osf-cache-dir))

(persp-mode)

(defun osf-query-persp-state-save ()
  (when (yes-or-no-p "Save perspective? ")
    (persp-state-save)))

(add-hook 'kill-emacs-hook #'osf-query-persp-state-save)

(defun osf-auto-save-persp ()
  (message "Auto save perspective...")
  (persp-state-save)
  (message "Auto save perspective...done"))

(defvar osf-auto-save-persp-timer nil)
(setq osf-auto-save-persp-timer
      (run-with-timer (* 5 60) (* 5 60) #'osf-auto-save-persp))

(with-eval-after-load 'consult
  (consult-customize consult--source-buffer :hidden t :default nil)
  (defvar osf--persp-consult-source
    (list :name "Perspective"
          :narrow ?s
          :category 'buffer
          :state #'consult--buffer-state
          :history 'buffer-name-history
          :default t
          :items
          (lambda ()
            (consult--buffer-query
             :sort 'identically-but-current
             :predicate '(lambda (buf) (persp-is-current-buffer buf t))
             :as #'buffer-name))))
  (add-to-list 'consult-buffer-sources 'osf--persp-consult-source))

(osf-leader-define-key 'global
  "b X" #'persp-kill-buffer*
  "TAB TAB" #'persp-switch
  "TAB x" #'persp-kill
  "TAB r" #'persp-rename
  "TAB a" #'persp-add-buffer
  "TAB A" #'persp-set-buffer
  "TAB d" #'persp-remove-buffer
  "TAB b" #'persp-switch-to-buffer
  "TAB B" #'persp-switch-to-scratch-buffer
  "TAB i" #'persp-import
  "TAB j" #'persp-next
  "TAB k" #'persp-prev
  "TAB m" #'persp-merge
  "TAB u" #'persp-unmerge
  "TAB g" #'persp-add-buffer-to-frame-global
  "TAB S" #'persp-state-save
  "TAB L" #'persp-state-load)

(provide 'osf-workspace)
