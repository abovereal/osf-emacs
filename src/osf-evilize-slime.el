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

(dolist (mode '(sldb-mode
                slime-inspector-mode
                slime-popup-buffer-mode
                slime-thread-control-mode
                slime-xref-mode))
  (evil-set-initial-state mode 'normal))

(osf-evil-define-key 'normal slime-parent-map
  "g d" #'slime-edit-definition
  "C-t" #'slime-pop-find-definition-stack)

(osf-evil-define-key 'normal slime-mode-map
  "K" #'slime-describe-symbol
  "g r" #'slime-who-references
  "g z z" #'slime-switch-to-output-buffer
  "g z s" #'slime-scratch)

(osf-evil-define-key 'normal slime-popup-buffer-mode-map
  "q" #'quit-window)

(osf-evil-define-key 'normal sldb-mode-map
  "RET" #'sldb-default-action
  "<backtab>" #'sldb-cycle
  "g ?" #'describe-mode
  "S" #'sldb-show-source
  "e" #'sldb-eval-in-frame
  "g :" #'slime-interactive-eval
  "d" #'sldb-pprint-eval-in-frame
  "D" #'sldb-disassemble
  "i" #'sldb-inspect-in-frame
  "C-j" #'sldb-down
  "C-k" #'sldb-up
  "M-j" #'sldb-details-down
  "M-k" #'sldb-details-up
  "g g" #'sldb-beginning-of-backtrace
  "G" #'sldb-end-of-backtrace
  "<tab>" #'sldb-toggle-details
  "g r" #'sldb-restart-frame
  "I" #'sldb-invoke-restart-by-name
  "R" #'sldb-return-from-frame
  "c" #'sldb-continue
  "s" #'sldb-step
  "n" #'sldb-next
  "o" #'sldb-out
  "b" #'sldb-break-on-return
  "a" #'sldb-abort
  "q" #'sldb-quit
  "B S" #'sldb-break-with-system-debugger
  "B B" #'sldb-break-with-default-debugger
  "P" #'sldb-print-condition
  "C" #'sldb-inspect-condition
  "0" #'sldb-invoke-restart-0
  "1" #'sldb-invoke-restart-1
  "2" #'sldb-invoke-restart-2
  "3" #'sldb-invoke-restart-3
  "4" #'sldb-invoke-restart-4
  "5" #'sldb-invoke-restart-5
  "6" #'sldb-invoke-restart-6
  "7" #'sldb-invoke-restart-7
  "8" #'sldb-invoke-restart-8
  "9" #'sldb-invoke-restart-9)

(osf-evil-define-key 'normal slime-inspector-mode-map
  "q" #'slime-inspector-quit
  "RET" #'slime-inspector-operate-on-point
  "C-o" #'slime-inspector-pop
  "C-i" #'slime-inspector-next
  "K" #'slime-inspector-describe
  "p" #'slime-inspector-pprint
  "e" #'slime-inspector-eval
  "C-p" #'slime-inspector-history
  "g r" #'slime-inspector-reinspect
  "g v" #'slime-inspector-toggle-verbose
  "<tab>" #'slime-inspector-next-inspectable-object
  "<backtab>" #''slime-inspector-previous-inspectable-object
  "g d" #'slime-inspector-show-source
  "g R" #'slime-inspector-fetch-all)

(osf-evil-define-key 'normal slime-xref-mode-map
  "RET" #'slime-goto-xref
  "g o" #'slime-show-xref
  "g j" #'slime-xref-next-line
  "g k" #'slime-xref-prev-line
  "C-j" #'slime-xref-next-line
  "C-k" #'slime-xref-prev-line
  "g c" #'slime-recompile-xref
  "g C" #'slime-recompile-all-xrefs)

(osf-evil-define-key 'normal slime-thread-control-mode-map
  "a" #'slime-thread-attach
  "d" #'slime-thread-debug
  "x" #'slime-thread-kill
  "g r" #'slime-update-threads-buffer)

(with-eval-after-load 'slime-repl
  (osf-evil-define-key '(normal insert) slime-repl-mode-map
    "C-j" #'slime-repl-next-prompt
    "C-k" #'slime-repl-previous-prompt
    "C-p" #'slime-repl-previous-input
    "C-n" #'slime-repl-next-input)

  (osf-evil-define-key 'normal slime-repl-mode-map
    "g z s" #'slime-scratch)
  
  (define-key slime-repl-mode-map (kbd "<return>") nil))

(add-hook 'slime-popup-buffer-mode-hook #'evil-normalize-keymaps)

(provide 'osf-evilize-slime)
