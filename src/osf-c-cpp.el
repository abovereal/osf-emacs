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

(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

(straight-use-package '(google-c-style :source emacsmirror-mirror))

(with-eval-after-load 'cc-mode
  (require 'google-c-style)
  (c-add-style "google" google-c-style))

(straight-use-package '(cmake-mode :source emacsmirror-mirror))

(setq cmake-tab-width 4)

(with-eval-after-load 'cmake-mode
  (osf-local-leader-define-key cmake-mode-map
    "h h" #'cmake-help
    "h c" #'cmake-help-command
    "h v" #'cmake-help-variable
    "h p" #'cmake-help-property
    "h m" #'cmake-help-module
    "h l" #'cmake-help-list-commands))

(straight-use-package 'cmake-font-lock)

;; FIXME This only partially cover styles I want.
(with-eval-after-load 'cc-styles
  (c-add-style
   "uniform"
   `((c-recognize-knr-p . nil)
     (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
     (c-basic-offset . 4)
     (indent-tabs-mode . nil)
     (c-comment-only-line-offset . 0)
     (c-hanging-braces-alist . ((defun-open before after)
                                (defun-close before after)
                                (class-open before after)
                                (class-close before after)
                                (inexpr-class-open after)
                                (inexpr-class-close before)
                                (namespace-open before after)
                                (inline-open before after)
                                (inline-close before after)
                                (block-open before after)
                                (block-close . c-snug-do-while)
                                (extern-lang-open before after)
                                (extern-lang-close before after)
                                (statement-case-open before after)
                                (substatement-open before after)))
     (c-hanging-colons-alist . ((case-label)
                                (label after)
                                (access-label after)
                                (member-init-intro before)
                                (inher-intro)))
     (c-hanging-semi&comma-criteria
      . (c-semi&comma-no-newlines-for-oneline-inliners
         c-semi&comma-inside-parenlist
         c-semi&comma-no-newlines-before-nonblanks))
     (c-indent-comments-syntactically-p . t)
     (comment-column . 40)
     (c-indent-comment-alist . ((other . (space . 2))))
     (c-cleanup-list . (brace-else-brace
                        brace-elseif-brace
                        brace-catch-brace
                        empty-defun-braces
                        defun-close-semi
                        list-close-comma
                        scope-operator))
     (c-offsets-alist . ((arglist-intro . +)
                         (func-decl-cont . ++)
                         (member-init-intro . +)
                         (inher-intro . ++)
                         (brace-list-open . 0)
                         (comment-intro . 0)
                         (arglist-close . c-lineup-arglist)
                         (topmost-intro . 0)
                         (block-open . 0)
                         (inline-open . 0)
                         (substatement-open . 0)
                         (statement-cont
                          .
                          (,(when (fboundp 'c-no-indent-after-java-annotations)
                              'c-no-indent-after-java-annotations)
                           ,(when (fboundp 'c-lineup-assignments)
                              'c-lineup-assignments)
                           ++))
                         (label . +)
                         (case-label . +)
                         (statement-case-open . 0)
                         (statement-case-intro . +) ; case w/o {
                         (access-label . -)
                         (innamespace . +)))))
  (setcdr (assoc 'other c-default-style) "uniform"))

(provide 'osf-c-cpp)
