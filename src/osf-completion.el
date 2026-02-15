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

(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

(with-eval-after-load 'minibuffer
  ;; Unbind "SPC" from `minibuffer-complete-word',
  ;; Bind `minibuffer-complete-word' to M-SPC instead.
  (osf-keymap-set minibuffer-local-completion-map
    "SPC" nil
    "M-SPC" #'minibuffer-complete-word))

(straight-use-package 'orderless)

(defun osf--orderless-consult-suffix-for-dollar ()
  "Regexp which matches the end of string with Consult tofu support
for the regexp dollar operator."
  (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
      (format "[%c-%c]*$"
              consult--tofu-char
              (+ consult--tofu-char consult--tofu-range -1))
    "$"))

(defvar osf-orderless-affix-dispatch-alist
  `((?# . ,#'orderless-regexp)
    (?! . ,#'orderless-without-literal)
    (?@ . ,#'orderless-annotation)))

(defun osf-orderless-affix-dispatch (component _index _total)
  (unless (string-empty-p component)
    (cond
     ;; Ignore single without-literal dispatcher
     ((and (= (length component) 1)
           (equal (aref component 0)
                  (car (rassq #'orderless-without-literal
                              osf-orderless-affix-dispatch-alist))))
      '(orderless-literal . ""))
     ;; Prefix
     ((when-let* ((style (alist-get (aref component 0)
                                   osf-orderless-affix-dispatch-alist)))
        (if (string-suffix-p "$" component)
            (cons style
                  (concat (substring component 1 -1)
                          (osf--orderless-consult-suffix-for-dollar)))
          (cons style (substring component 1)))))
     ;; Suffix
     ((when-let* ((style (alist-get (aref component (1- (length component)))
                                   osf-orderless-affix-dispatch-alist)))
        (if (and (>= (length component) 2)
                 (eq ?$ (aref component (- (length component) 2))))
            (cons style (concat (substring component 0 -2)
                                (osf--orderless-consult-suffix-for-dollar)))
          (cons style (substring component 0 -1))))))))

(setq completion-styles '(orderless basic partial-completion)
      orderless-matching-styles '(orderless-literal)
      orderless-style-dispatchers '(osf-orderless-affix-dispatch)
      completion-category-defaults nil
      completion-category-overrides nil)

(defvar osf-default-completion-styles
  (let ((sv (get 'completion-styles 'standard-value)))
    (and (consp sv)
         (condition-case nil
             (eval (car sv) t)
           (error completion-styles)))))

(with-eval-after-load 'company
  (defun osf--disable-orderless-in-company (orig-fun &rest args)
    "Diable orderless completion style when company is doing the completion."
    (let ((completion-styles osf-default-completion-styles))
      (apply orig-fun args)))
  (advice-add #'company--perform :around #'osf--disable-orderless-in-company))

(straight-use-package '(vertico :files (:defaults "extensions/*")
                                :includes (vertico-indexed)))

(setq vertico-multiline
      (cons #("\\\\n" 0 3 (face vertico-multiline))
            #("..." 0 3 (face vertico-multiline))))

(require 'vertico)

(osf-indexed-setup-keymap vertico-map)

(vertico-mode)

(vertico-indexed-mode)

(straight-use-package 'marginalia)

(marginalia-mode)

(osf-keymap-set minibuffer-local-map
  "M-A" #'marginalia-cycle)

(straight-use-package 'consult)

(with-eval-after-load 'consult
  (defun consult--buffer-sort-identically-but-current (buffers)
    "Sort identically but exclude the current buffer.
Most importantly, don't put visible buffers in the bottom of the list."
    ;; Rationale: split the current window into windows A and B, both
    ;; windows now display the same buffer B.
    ;; In window A, Invoke `consult-buffer' then press RET immediately
    ;; to switch to the most recent buffer R, view some content, then
    ;; use `consult-buffer' RET again to switch to the previous buffer
    ;; B. Without looking at the window B (focusing on window A), I
    ;; would expect that I would be switched to the buffer B, but
    ;; since the buffer B is visible right now in window B, I would
    ;; actucally go into a unexpected buffer.
    (let ((current (current-buffer)))
      (nconc (delq current buffers) (list current))))

  (plist-put consult--source-buffer :items
             (lambda ()
               (consult--buffer-query
                :sort 'identically-but-current
                :as #'buffer-name)))

  (osf-annotate-within-function consult--find-file-temporarily-1)
  (with-eval-after-load 'saveplace
    (defun osf--consult-inhibit-save-place-to-alist-in-preview (fn &rest args)
      (unless osf-within-consult--find-file-temporarily-1?
        (apply fn args)))
    (advice-add #'save-place-to-alist
                :around #'osf--consult-inhibit-save-place-to-alist-in-preview)))

(with-eval-after-load 'slime-repl
  (with-eval-after-load 'consult
    (setf (alist-get 'slime-repl-mode consult-mode-histories)
          '(slime-repl-input-history)))
  (osf-evil-define-key 'insert slime-repl-mode-map
    "M-Y" #'consult-history))

(with-eval-after-load 'esh-mode
  (osf-evil-define-key 'insert eshell-mode-map
    "M-Y" #'consult-history))

(with-eval-after-load 'comint
  (osf-evil-define-key 'insert comint-mode-map
    "M-Y" #'consult-history))

(osf-evil-define-key 'insert minibuffer-local-map
  "M-Y" #'consult-history)

(osf-evil-define-key 'insert evil-ex-search-keymap
  "M-Y" #'consult-history)

(osf-leader-define-key 'global
  "b b" #'consult-buffer)

(osf-evil-define-key 'insert 'global
  "M-y" #'consult-yank-from-kill-ring)

(with-eval-after-load 'org
  (osf-local-leader-define-key org-mode-map
    "/ h" #'consult-org-heading))

(defun osf-completion-in-region-function (&rest args)
  (apply (if vertico-mode
             #'consult-completion-in-region
           #'completion--in-region)
         args))
(setq completion-in-region-function #'osf-completion-in-region-function)

(straight-use-package 'consult-dir)
(setq consult-dir-jump-file-command #'consult-fd)
(with-eval-after-load 'consult-dir
  (defun osf-consult-dir-jump-file ()
    "Like `consult-dir-jump-file',
but use `default-directory' if not in minibuffer."
    (interactive)
    (let ((in-minibuffer (minibufferp))
          (dir default-directory)
          (search ""))
      (when in-minibuffer
        (let ((mc (substitute-in-file-name (minibuffer-contents))))
          (setq dir (file-name-directory mc))
          (setq search (file-name-nondirectory mc))))
      (run-at-time 0 nil
                   (lambda () (funcall #'consult-fd
                                       dir
                                       (concat search
                                               (unless (string-empty-p search)
                                                 (plist-get (consult--async-split-style)
                                                            :initial))))))
      (when in-minibuffer (abort-recursive-edit)))))
(autoload #'osf-consult-dir-jump-file "consult-dir")
(osf-leader-define-key 'global
  "f d" #'consult-dir
  "f j" #'osf-consult-dir-jump-file)

(straight-use-package 'company)

(setq company-idle-delay 0.1
      company-minimum-prefix-length 1
      company-show-quick-access t
      company-dabbrev-ignore-case t
      company-dabbrev-downcase nil
      company-dabbrev-ignore-buffers (rx (or (regexp "\\`[ *]")
                                             (seq "." (regexp "[pP][dD][fF]") string-end)))
      company-tooltip-align-annotations t
      company-require-match 'never
      company-global-modes
      '(not eshell-mode shell-mode term-mode)
      company-backends '(company-capf
                         company-cmake
                         company-files
                         (company-dabbrev-code
                          company-gtags
                          company-etags
                          company-keywords)
                         company-dabbrev))

(require 'company)

(evil-declare-change-repeat #'company-complete-quick-access)

(defun osf--ensure-company-keymaps-not-been-overridden ()
  ;; Note:
  ;; Check if `company-emulation-alist' is in
  ;; `emulation-mode-map-alists', if true, call
  ;; `company-ensure-emulation-alist' to ensure
  ;; `company-emulation-alist' is the first item of
  ;; `emulation-mode-map-alists', thus has a higher
  ;; priority than keymaps of evil-mode.
  ;; We raise the priority of company-mode keymaps
  ;; unconditionally even when completion is not
  ;; activated. This should not cause problems,
  ;; because when completion is activated, the value of
  ;; `company-emulation-alist' is ((t . company-my-keymap)),
  ;; when completion is not activated, the value is ((t . nil)).
  (when (memq 'company-emulation-alist emulation-mode-map-alists)
    (company-ensure-emulation-alist)))

(add-hook 'evil-local-mode-hook
          #'osf--ensure-company-keymaps-not-been-overridden)

(defun osf-company-complete-by-completing-read ()
  "Complete current company candidates by `completing-read'."
  (interactive)
  (unless company-candidates (user-error "No company candidates avaiable"))
  (when-let* ((cand (completing-read "Candidate: " company-candidates)))
    (company-finish cand)))

(osf-keymap-set company-active-map
  "M-<" #'company-select-first
  "M->" #'company-select-last
  "M-\\" #'osf-company-complete-by-completing-read)

(global-company-mode 1)

(defun osf-switch-to-completions-or-scroll-down (&optional arg)
  (interactive "^P")
  (cond (arg (scroll-down-command arg))
        (t (if-let* ((window (get-buffer-window "*Completions*" 0)))
               (if (eq (selected-window) window)
                   (scroll-down-command)
                 (switch-to-completions))
             (scroll-down-command)))))

(osf-keymap-global-set
  "M-v" #'osf-switch-to-completions-or-scroll-down)

(provide 'osf-completion)
