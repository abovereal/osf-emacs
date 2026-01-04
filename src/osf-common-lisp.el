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

(straight-use-package 'slime)

(setq slime-net-coding-system 'utf-8-unix
      slime-contribs '(slime-fancy))

(with-eval-after-load 'slime
  (when (executable-find "sbcl")
    (add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")))
    (add-to-list 'slime-lisp-implementations '(sbcl-2g ("sbcl" "--dynamic-space-size" "2048")))
    (add-to-list 'slime-lisp-implementations '(sbcl-4g ("sbcl" "--dynamic-space-size" "4096")))
    (setq slime-default-lisp 'sbcl))

  (when (executable-find "ros")
    (add-to-list 'slime-lisp-implementations '(roswell ("ros" "run")))
    (add-to-list 'slime-lisp-implementations '(roswell-2g ("ros" "dynamic-space-size=2048" "run")))
    (add-to-list 'slime-lisp-implementations '(roswell-4g ("ros" "dynamic-space-size=4096" "run")))
    (setq slime-default-lisp 'roswell))

  (let ((local-hyperspec-dir (expand-file-name "~/HyperSpec/")))
    (when (file-directory-p local-hyperspec-dir)
      (setq common-lisp-hyperspec-root
            (concat "file:///" local-hyperspec-dir))))

  (defun osf--slime-auto-select-popup-buffer ()
    (add-hook
     'window-buffer-change-functions
     #'osf--slime-auto-select-popup-buffer-change-function nil t))
  (add-hook 'slime-popup-buffer-mode-hook #'osf--slime-auto-select-popup-buffer)

  (defun osf--slime-auto-select-popup-buffer-change-function (window)
    (unless (eq (selected-window) window)
      (select-window window))
    (remove-hook
     'window-buffer-change-functions
     #'osf--slime-auto-select-popup-buffer-change-function t))

  (evil-set-command-property #'slime-edit-definition :jump t))

(straight-use-package 'slime-company)
(setq slime-company-completion 'fuzzy)
(add-to-list 'slime-contribs 'slime-company)

(defun osf--slime-setup-leader-key-bindings (map)
  (osf-local-leader-define-key map
    "e e" #'osf-slime-eval-last-expression-in-repl
    "e d" #'osf-slime-eval-defun-in-repl

    "h f" #'slime-describe-function
    "h s" #'slime-describe-symbol
    "h h" #'slime-documentation-lookup
    "h H" #'slime-hyperspec-lookup
    "h ~" #'common-lisp-hyperspec-format
    "h g" #'common-lisp-hyperspec-glossary-term
    "h #" #'common-lisp-hyperspec-lookup-reader-macro
    "h a a" #'slime-apropos
    "h a A" #'slime-apropos-all
    "h a p" #'slime-apropos-package

    "q q" #'slime-quit-lisp
    "q r" #'slime-restart-inferior-lisp))

(defun osf--slime-repl-setup-leader-key-bindings (map)
  (osf-local-leader-define-key map
    "c b" #'slime-repl-clear-buffer
    "c o" #'slime-repl-clear-output
    "p s" #'slime-repl-set-package

    "i e" #'slime-repl-inspect))

(with-eval-after-load 'slime
  (osf--slime-setup-leader-key-bindings slime-mode-map))

(with-eval-after-load 'slime-repl
  (osf--slime-setup-leader-key-bindings slime-repl-mode-map)
  (osf--slime-repl-setup-leader-key-bindings slime-repl-mode-map)

  (defun osf-slime-repl-return-eval-at-end ()
    "Like `slime-repl-return',
but only eval the form if the current evil state is normal state or
the current point is at the end of the repl buffer.
Otherwise, just `newline-and-indent'."
    (interactive)
    (call-interactively (if (or (eq evil-state 'normal)
                                (= (point) (point-max)))
                            #'slime-repl-return
                          #'newline-and-indent)))

  (defun osf-slime-dwim-repl-return ()
    "If around a presentation, inspect the presentation,
otherwise, fallback to `osf-slime-repl-return-eval-at-end'."
    (interactive)
    (call-interactively
     (if (slime-presentation-around-or-before-point-p)
         #'slime-inspect-presentation-at-point
       #'osf-slime-repl-return-eval-at-end)))

  (osf-evil-define-key '(normal insert) slime-repl-mode-map
    "RET" #'osf-slime-dwim-repl-return))

(with-eval-after-load 'slime
  (defun osf-slime-eval-last-expression-in-repl (prefix)
    "Like `slime-eval-last-expression-in-repl', but doesn't change selected window."
    (interactive "P")
    (let ((w (selected-window)))
      (slime-eval-last-expression-in-repl prefix)
      (select-window w)))
  (osf-evil-define-key '(insert normal) slime-mode-map
    "M-e" #'osf-slime-eval-last-expression-in-repl))

(defun osf--common-lisp-hyperspec-read-symbol-name (&optional symbol-at-point)
  "Like `common-lisp-hyperspec-read-symbol-name',
but:
1. Always ask for completion rather than go directly to the HyperSpec.
2. Use the DEF argument of `completing-read' instead of the INITIAL-INPUT."
  (let* ((symbol-at-point (or symbol-at-point (thing-at-point 'symbol)))
	     (stripped-symbol (and symbol-at-point
			                   (common-lisp-hyperspec--strip-cl-package
				                (downcase symbol-at-point)))))
    (completing-read "Look up symbol in Common Lisp HyperSpec: "
                     common-lisp-hyperspec--symbols nil t
                     nil
                     'common-lisp-hyperspec-history stripped-symbol)))

(advice-add #'common-lisp-hyperspec-read-symbol-name
            :override #'osf--common-lisp-hyperspec-read-symbol-name)

(provide 'osf-common-lisp)
