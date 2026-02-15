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

(straight-use-package 'smartparens)

(setq sp-highlight-pair-overlay nil
      sp-highlight-wrap-overlay nil
      sp-highlight-wrap-tag-overlay nil)

(require 'smartparens-config)

(sp-with-modes '(js-mode
                 css-mode
                 rust-mode
                 java-mode
                 glsl-mode
                 zig-mode
                 go-mode
                 typescript-ts-mode
                 tsx-ts-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "(" nil :post-handlers '(("||\n[i]" "RET"))))


(with-eval-after-load 'smartparens-org
  (defun osf-sp-point-after-right-pair (id action _context)
    (when (memq action '(insert escape))
      (sp--looking-back-p (rx (or ")" "}" "]") (literal id)))))
  (sp-with-modes 'org-mode
    (sp-local-pair
     "_" "_"
     :unless '(sp-point-after-word-p osf-sp-point-after-right-pair))))

(smartparens-global-mode)

(defvar osf-smartparens-lisp-mode-map (make-sparse-keymap))

(osf-evil-define-key 'normal osf-smartparens-lisp-mode-map
  ">" #'sp-forward-slurp-sexp
  "<" #'sp-forward-barf-sexp
  "[" #'sp-backward-slurp-sexp
  "]" #'sp-backward-barf-sexp
  "M-j" #'sp-forward-parallel-sexp
  "M-k" #'sp-backward-parallel-sexp
  "M-h" #'sp-backward-up-sexp
  "M-r" #'sp-raise-sexp
  "M-s" #'sp-splice-sexp
  "M-S" #'sp-split-sexp
  "M-(" #'sp-wrap-round
  "M-q" #'sp-indent-defun
  "M-w" #'sp-clone-sexp)

(defun osf-smartparens-eval-expression-ret ()
  (interactive)
  (if (or (evil-normal-state-p)
          (and (evil-insert-state-p)
               (or (= (point) (minibuffer-prompt-end))
                   (= (point) (point-max)))))
      (read--expression-try-read)
    (newline-and-indent)))

(defvar osf-smartparens-eval-expression-mode-map (make-sparse-keymap))
(set-keymap-parent osf-smartparens-eval-expression-mode-map osf-smartparens-lisp-mode-map)

(osf-evil-define-key '(insert normal) osf-smartparens-eval-expression-mode-map
  "RET" #'osf-smartparens-eval-expression-ret)

(define-minor-mode osf-smartparens-lisp-mode
  "Provide separate keymap for Lisp modes."
  :init-value nil
  :lighter nil
  :group 'smartparens
  :keymap osf-smartparens-lisp-mode-map)

(define-minor-mode osf-smartparens-eval-expression-mode
  "Provide separate keymap for `eval-expression'."
  :init-value nil
  :lighter nil
  :group 'smartparens
  :keymap osf-smartparens-eval-expression-mode-map)

(defun osf-enable-smartparens-lisp-mode ()
  (smartparens-mode 1)
  (osf-smartparens-lisp-mode 1)
  (evil-normalize-keymaps))

(defun osf-enable-smartparens-eval-expression-mode ()
  (smartparens-mode 1)
  (osf-smartparens-eval-expression-mode 1))

(dolist (hook '(emacs-lisp-mode-hook
                lisp-interaction-mode-hook
                lisp-mode-hook
                lisp-data-mode-hook
                scheme-mode-hook
                slime-repl-mode-hook))
  (add-hook hook #'osf-enable-smartparens-lisp-mode))

(defun osf--enable-smartparens-for-eval-expression ()
  (when (eq this-command 'eval-expression)
    ;; Use `lisp-indent-line' to fix the weird indentation.
    (setq-local indent-line-function #'lisp-indent-line)
    (osf-enable-smartparens-eval-expression-mode)))
(add-hook 'minibuffer-setup-hook #'osf--enable-smartparens-for-eval-expression)

(provide 'osf-smartparens)
