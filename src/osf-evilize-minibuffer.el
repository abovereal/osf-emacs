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

(defun osf--evilize-minibuffer-setup ()
  (set (make-local-variable 'evil-echo-state) nil)
  (evil-insert 1))
(add-hook 'minibuffer-setup-hook #'osf--evilize-minibuffer-setup)

(dolist (map (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map
                   evil-ex-completion-map
                   evil-ex-search-keymap))
  (osf-evil-define-key 'normal map
    "<escape>" #'abort-recursive-edit
    "RET" #'exit-minibuffer

    ;; Use isearch search module in minibuffer so that we can search
    ;; minibuffer. The evil-ex search module will bring recursive
    ;; minibuffer thus we can't search the original minibuffer.
    "/" #'evil-search-forward
    "?" #'evil-search-backward
    "n" #'evil-search-next
    "N" #'evil-search-previous)

  (osf-evil-define-key '(normal insert) map
    "C-n" #'next-complete-history-element
    "C-p" #'previous-complete-history-element
    "M-n" #'next-history-element
    "M-p" #'previous-history-element))

(provide 'osf-evilize-minibuffer)
