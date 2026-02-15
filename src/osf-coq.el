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

(straight-use-package 'proof-general)
(setq proof-splash-enable nil)

(with-eval-after-load 'coq-mode
  ;; Fix undo/redo, adapt from https://github.com/ProofGeneral/PG/issues/430#issuecomment-604317650
  (defun osf-pg-in-protected-region? ()
    (< (point) (proof-queue-or-locked-end)))

  (defun coq-wrap-edit (action)
    (if (or (not proof-locked-span)
            (equal (proof-queue-or-locked-end) (point-min)))
        (call-interactively action)
      (call-interactively action)
      (when (osf-pg-in-protected-region?)
        (proof-goto-point))))

  (defun osf-coq-undo ()
    (interactive)
    (coq-wrap-edit #'evil-undo))

  (defun osf-coq-redo ()
    (interactive)
    (coq-wrap-edit #'evil-redo))

  (osf-local-leader-define-key coq-mode-map
    "j" #'proof-assert-next-command-interactive
    "k" #'proof-undo-last-successful-command
    "DEL" #'proof-undo-and-delete-last-successful-command
    "p" #'proof-prf
    "." #'proof-goto-point
    "H" #'proof-goto-command-start
    "L" #'proof-goto-command-end
    "i" #'prog-indent-sexp
    "I" #'prog-fill-reindent-defun
    "w l" #'proof-layout-windows)

  (osf-evil-define-key 'normal coq-mode-map
    "u" #'osf-coq-undo
    "C-r" #'osf-coq-redo))

(straight-use-package 'company-coq)

(add-hook 'coq-mode-hook #'company-coq-mode)

(with-eval-after-load 'company-coq
  (unless (osf-font-exists? "Symbola")
    (add-to-list 'company-coq-disabled-features 'prettify-symbols)
    (add-to-list 'company-coq-disabled-features 'smart-subscripts)))

(provide 'osf-coq)
