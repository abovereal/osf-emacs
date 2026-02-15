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

(dolist (mode '(git-rebase-mode
                magit-mode
                magit-status-mode
                magit-stash-mode
                magit-stashes-mode
                magit-diff-mode
                magit-cherry-mode
                magit-log-mode
                magit-log-select-mode
                magit-process-mode
                magit-refs-mode
                magit-reflog-mode
                magit-revision-mode
                magit-repolist-mode))
  (evil-set-initial-state mode 'normal))

(apply
 #'osf-evil-define-key '(normal visual) magit-mode-map
 `("g" nil
   "h" evil-backward-char
   "l" evil-forward-char
   "H" magit-dispatch
   "L" magit-log
   "C-l" magit-log-refresh
   "j" evil-next-visual-line
   "k" evil-previous-visual-line
   "g g" evil-goto-first-line
   "G" evil-goto-line
   "C-d" evil-scroll-down
   ":" evil-ex
   "q" magit-mode-bury-buffer
   "g r" magit-refresh
   "g R" magit-refresh-all
   "x" magit-delete-thing
   "X" magit-file-untrack
   "p" magit-push
   "o" magit-reset-quickly
   "O" magit-reset
   "-" magit-revert-no-commit
   "_" magit-revert
   "=" magit-diff-less-context
   "+" magit-diff-more-context
   "M-+" magit-diff-default-context
   "'" magit-submodule
   "\"" magit-subtree
   ,evil-toggle-key evil-emacs-state
   "Z" magit-stash
   "v" evil-visual-line
   "V" evil-visual-line
   "C-w" evil-window-map
   "y" nil
   "y y" osf-evil-yank-whole-line
   "y s" magit-copy-section-value
   "y b" magit-copy-buffer-revision
   "g '" magit-show-refs
   "0" evil-beginning-of-line
   "$" evil-end-of-line
   "`" magit-process-buffer
   ,@(when evil-want-C-u-scroll
       '("C-u" evil-scroll-up))
   ,@(cond ((eq evil-search-module 'evil-search)
            '("/" evil-ex-search-forward
              "?" evil-ex-search-backward
              "n" evil-ex-search-next
              "N" evil-ex-search-previous))
           (t
            '("/" evil-search-forward
              "?" evil-search-backward
              "n" evil-search-next
              "N" evil-search-previous)))))

(osf-evil-define-key 'visual magit-mode-map
  "y" #'magit-copy-section-value)

(osf-evil-define-key '(normal visual) magit-status-mode-map
 "g s" #'magit-jump-to-staged
 "g u" #'magit-jump-to-unstaged
 "g t" #'magit-jump-to-tracked
 "g n" #'magit-jump-to-untracked
 "g z" #'magit-jump-to-stashes
 "g f u" #'magit-jump-to-unpulled-from-upstream
 "g f p" #'magit-jump-to-unpulled-from-pushremote
 "g p u" #'magit-jump-to-unpushed-to-pushremote
 "g p p" #'magit-jump-to-unpushed-to-pushremote
 "g d" #'magit-jump-to-diffstat-or-diff)

(osf-evil-define-key '(normal visual) magit-section-mode-map
  "<tab>" #'magit-section-toggle
  "<backtab>" #'magit-section-cycle-global
  "<C-tab>" #'magit-section-cycle
  "C-j" #'magit-section-forward
  "C-k" #'magit-section-backward
  "]" #'magit-section-forward-sibling
  "[" #'magit-section-backward-sibling
  "g h" #'magit-section-up
  "z 1" #'magit-section-show-level-1-all
  "z 2" #'magit-section-show-level-2-all
  "z 3" #'magit-section-show-level-3-all
  "z 4" #'magit-section-show-level-4-all
  "z a" #'magit-section-toggle
  "z c" #'magit-section-hide
  "z C" #'magit-section-hide-children
  "z o" #'magit-section-show
  "z O" #'magit-section-show-children)

(osf-evil-define-key '(normal visual) magit-blob-mode-map
  "C-j" #'magit-blob-next
  "C-k" #'magit-blob-previous)

(osf-keymap-set magit-blob-mode-map
  "n" nil)

(osf-evil-define-key 'normal magit-blame-read-only-mode-map
  "j" #'evil-next-visual-line
  "k" #'evil-previous-visual-line
  "C-j" #'magit-blame-next-chunk
  "C-k" #'magit-blame-previous-chunk
  "M-j" #'magit-blame-next-chunk-same-commit
  "M-k" #'magit-blame-previous-chunk-same-commit)

(apply
 #'osf-evil-define-key '(normal visual) magit-blame-read-only-mode-map
 `(,@(cond ((eq evil-search-module 'evil-search)
            '("/" evil-ex-search-forward
              "?" evil-ex-search-backward
              "n" evil-ex-search-next
              "N" evil-ex-search-previous))
           (t
            '("/" evil-search-forward
              "?" evil-search-backward
              "n" evil-search-next
              "N" evil-search-previous)))))

(dolist (map (list magit-blame-mode-map magit-blame-read-only-mode-map))
  (osf-evil-define-key 'normal map
    "q" #'magit-blame-quit))

(defvar osf-evilize-magit-popup-keys-remapped nil)
(unless osf-evilize-magit-popup-keys-remapped
  (let ((popup-remaps
         '((magit-branch "x" "X")
           (magit-branch "k" "x")
           (magit-dispatch "o" "'")
           (magit-dispatch "O" "\"")
           (magit-dispatch "V" "_")
           (magit-dispatch "X" "O")
           (magit-dispatch "v" "-")
           (magit-dispatch "k" "x")
           (magit-remote "k" "x")
           ;; FIXME: Two actions with same keys but with different apply
           ;; conditions, how to remap properly?
           (magit-revert "V" "_")
           (magit-revert "V" "_")
           (magit-tag "k" "x"))))
    (pcase-dolist (`(,popup ,from ,to) popup-remaps)
      (transient-suffix-put popup from :key to)))
  (setq osf-evilize-magit-popup-keys-remapped t))

(dolist (map (list magit-mode-map
                   magit-status-mode-map
                   magit-blob-mode-map
                   magit-diff-mode-map
                   magit-cherry-mode-map
                   magit-log-mode-map
                   magit-log-select-mode-map
                   magit-log-read-revs-map
                   magit-refs-mode-map
                   magit-reflog-mode-map
                   magit-process-mode-map))
  (evil-make-overriding-map map))

(evil-make-overriding-map magit-blame-read-only-mode-map 'normal)

(dolist (cmd '(magit-section-forward-sibling
               magit-section-forward
               magit-section-backward-sibling
               magit-section-backward
               magit-section-up))
  (evil-set-command-property cmd :keep-visual t))

;; Need to refresh evil keymaps when blame mode is entered.
(add-hook 'magit-blame-mode-hook #'evil-normalize-keymaps)

(define-minor-mode osf-evilize-magit-toggle-text-minor-mode
  "Minor mode used to enabled toggle key in `text-mode' after
using `osf-evilize-magit-toggle-text-mode'."
  :keymap (make-sparse-keymap))

(defvar osf-evilize-magit-last-mode nil
  "Used to store last magit mode before entering text mode using
`osf-evilize-magit-toggle-text-mode'.")

(defun osf-evilize-magit-toggle-text-mode ()
  "Switch to `text-mode' and back from magit buffers."
  (interactive)
  (cond
   ((derived-mode-p 'magit-mode)
    (setq osf-evilize-magit-last-mode major-mode)
    (message "Switching to text-mode")
    (text-mode)
    (osf-evilize-magit-toggle-text-minor-mode 1)
    (evil-normalize-keymaps))
   ((and (eq major-mode 'text-mode)
         (functionp osf-evilize-magit-last-mode))
    (message "Switching to %s" osf-evilize-magit-last-mode)
    (osf-evilize-magit-toggle-text-minor-mode -1)
    (evil-normalize-keymaps)
    (funcall osf-evilize-magit-last-mode)
    (magit-refresh)
    (evil-change-state 'normal))
   (t
    (user-error "osf-evilize-magit-toggle-text-mode unexpected state"))))

(osf-evil-define-key 'normal osf-evilize-magit-toggle-text-minor-mode-map
  "C-t" #'osf-evilize-magit-toggle-text-mode)
(osf-evil-define-key 'normal magit-mode-map
  "C-t" #'osf-evilize-magit-toggle-text-mode)

(evil-set-initial-state 'magit-repolist-mode 'normal)
(osf-evil-define-key 'normal magit-repolist-mode-map
  "q" #'quit-window
  "m" #'magit-repolist-mark
  "u" #'magit-repolist-unmark
  "f" #'magit-repolist-fetch
  "RET" #'magit-repolist-status
  "g r" #'magit-list-repositories)
(add-hook 'magit-repolist-mode-hook #'evil-normalize-keymaps)

(evil-set-initial-state 'magit-submodule-list-mode 'normal)
(osf-evil-define-key 'normal magit-submodule-list-mode-map
  "q" #'quit-window
  "RET" #''magit-repolist-status
  "g r"  #''magit-list-submodules)
(add-hook 'magit-submodule-list-mode-hook #'evil-normalize-keymaps)

(osf-keymap-set magit-file-section-map "C-j" nil)
(osf-keymap-set magit-hunk-section-map "C-j" nil)

(with-eval-after-load 'git-rebase
  (defvar git-rebase-mode-map)
  (defvar git-rebase-show-instructions)
  (defvar git-rebase-comment-re)

  (defvar osf-evilize-magit-git-rebase-mode-key-binding-specs
    '(("p" git-rebase-pick "pick = use commit")
      ("r" git-rebase-reword "reword = use commit, but edit the commit message")
      ("e" git-rebase-edit "edit = use commit, but stop for amending")
      ("s" git-rebase-squash "squash = use commit, but meld into previous commit")
      ("f" git-rebase-fixup "fixup = like \"squash\", but discard this commit's log message")
      ("x" git-rebase-exec "exec = run command (the rest of the line) using shell")
      ("d" git-rebase-kill-line "drop = remove commit")
      ("u" git-rebase-undo "undo last change")
      (nil with-editor-finish "tell Git to make it happen")
      (nil with-editor-cancel "tell Git that you changed your mind, i.e. abort")
      ("j" evil-next-visual-line "move point to next line")
      ("k" evil-previous-visual-line "move point to previous line")
      ("M-k" git-rebase-move-line-up "move the commit at point up")
      ("M-j" git-rebase-move-line-down "move the commit at point down")
      (nil git-rebase-show-commit "show the commit at point in another buffer")))

  (pcase-dolist
      (`(,key ,cmd, _) osf-evilize-magit-git-rebase-mode-key-binding-specs)
    (when key
      (osf-evil-define-key 'normal git-rebase-mode-map
        key cmd)))

  (evil-make-overriding-map git-rebase-mode-map 'normal)

  (defun osf-evilize-magit-git-rebase-mode-show-keybindings ()
    "Modify the \"Commands:\" section of the comment Git generates
at the bottom of the file so that in place of the one-letter
abbreviation for the command, it shows the command's keybinding.
By default, this is the same except for the \"pick\" command."
    (let ((inhibit-read-only t)
          (aux-map (evil-get-auxiliary-keymap git-rebase-mode-map 'normal)))
      (save-excursion
        (goto-char (point-min))
        (when (and git-rebase-show-instructions
                   (re-search-forward
                    (concat git-rebase-comment-re "\\s-+p, pick")
                    nil t))
          (goto-char (line-beginning-position))
          (flush-lines (concat "^" (regexp-quote comment-start) ".+ = "))
          (pcase-dolist
              (`(,key ,cmd ,desc)
               osf-evilize-magit-git-rebase-mode-key-binding-specs)
            (insert
             (format (propertize "%s %s %s\n"
                                 'font-lock-face 'font-lock-comment-face)
                     comment-start
                     (string-pad
                      (if (and key
                               (eq (lookup-key aux-map (kbd key)) cmd))
                          key
                        (replace-regexp-in-string
                         "<normal-state> " ""
                         (substitute-command-keys
                          (format "\\[%s]" cmd))))
                      8)
                     desc)))))))
  (remove-hook 'git-rebase-mode-hook
               #'git-rebase-mode-show-keybindings)
  (add-hook 'git-rebase-mode-hook
            'osf-evilize-magit-git-rebase-mode-show-keybindings))

;; Fix: the active region of visual-line-mode contains an extra newline
;; and the end, cause `magit-diff-scope' return wrong scope and won't
;; operate on selected files/hunks, only single file/hunk is operated.

(defvar osf-evilize-magit-in-visual-pre-command nil)

(defun evilize--magit-visual-pre-command-ad (fn &rest args)
  (let ((osf-evilize-magit-in-visual-pre-command t))
    (apply fn args)))

(defun evilize--magit-visual-expand-region-ad (arglist)
  ;; pretend that the command has the :exclude-newline property by rewriting the
  ;; EXCLUDE-NEWLINE arg to this function
  (cons (and osf-evilize-magit-in-visual-pre-command
             (null (car arglist))
             (eq (evil-visual-type) 'line)
             (derived-mode-p 'magit-mode))
        ;; shouldn't be necessary, but this will prevent it from failing if an
        ;; arg is added.
        (cdr arglist)))

(advice-add #'evil-visual-pre-command
            :around #'evilize--magit-visual-pre-command-ad)
(advice-add #'evil-visual-expand-region
            :filter-args #'evilize--magit-visual-expand-region-ad)

;; without this set-mark-command activates visual-state which is just annoying
;; and introduces possible bugs
(defun osf-evilize-magit-remove-visual-activate-hook ()
  (when (derived-mode-p 'magit-mode)
    (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t)))
(add-hook 'evil-local-mode-hook
          #'osf-evilize-magit-remove-visual-activate-hook)

(provide 'osf-evilize-magit)
