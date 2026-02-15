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

(evil-set-initial-state 'pdf-view-mode 'normal)

(add-hook 'pdf-view-mode-hook #'osf-evil-disable-evil-adjust-cursor)

(defvar-local osf--evilize-pdf-view-isearch-highlight-matches nil)

(defun osf-evilize-pdf-view-isearch-cleanup-highlight ()
  (setq osf--evilize-pdf-view-isearch-highlight-matches nil)
  (pdf-view-redisplay))

(defun osf--evilize-pdf-view-isearch-hl-matches-controllable-highlight
    (fn current matches &optional occur-hack-p)
  (funcall fn current matches
           (or osf--evilize-pdf-view-isearch-highlight-matches occur-hack-p)))

(advice-add #'pdf-isearch-hl-matches
            :around #'osf--evilize-pdf-view-isearch-hl-matches-controllable-highlight)

(defvar-local osf--evilize-pdf-view-isearch-forward? t)

(defun osf-evilize-pdf-view-isearch-forward (&optional regexp-p no-recursive-edit)
  "Like `isearch-forward', but remember the previous search direction."
  (interactive "P\np")
  (setq osf--evilize-pdf-view-isearch-forward? t)
  (isearch-forward regexp-p no-recursive-edit))

(defun osf-evilize-pdf-view-isearch-backward (&optional regexp-p no-recursive-edit)
  "Like `isearch-backward', but remember the previous search direction."
  (interactive "P\np")
  (setq osf--evilize-pdf-view-isearch-forward? nil)
  (isearch-backward regexp-p no-recursive-edit))

(defun osf-evilize-pdf-view-isearch-repeat-forward (&optional arg)
  (interactive "P")
  (setq osf--evilize-pdf-view-isearch-highlight-matches t)
  (if osf--evilize-pdf-view-isearch-forward?
      (isearch-repeat-forward arg)
    (isearch-repeat-backward arg)))

(defun osf-evilize-pdf-view-isearch-repeat-backward (&optional arg)
  (interactive "P")
  (setq osf--evilize-pdf-view-isearch-highlight-matches t)
  (if osf--evilize-pdf-view-isearch-forward?
      (isearch-repeat-backward arg)
    (isearch-repeat-forward arg)))

(evil-define-command osf-evilize-pdf-view-force-normal-state ()
  :repeat abort
  :suppress-operator t
  (evil-normal-state)
  (osf-evilize-pdf-view-isearch-cleanup-highlight))

(defun osf--evilize-pdf-view-reset-isearch-highlight-matches ()
  (setq osf--evilize-pdf-view-isearch-highlight-matches nil))

(advice-add #'pdf-isearch-mode-initialize
            :before #'osf--evilize-pdf-view-reset-isearch-highlight-matches)

(defun osf--evilize-pdf-view-isearch-mode-cleanup ()
  "Don't cleanup highlight if search successfully."
  (pdf-isearch-active-mode -1)
  (when (osf-isearch-failed?)
    (pdf-view-redisplay)))

(advice-add #'pdf-isearch-mode-cleanup
            :override #'osf--evilize-pdf-view-isearch-mode-cleanup)

(defun osf--evilize-pdf-view-highlight-states-in-mode-line (orig-fun &optional state)
  (let ((tag (funcall orig-fun state)))
    (if (and (stringp tag)
             (derived-mode-p 'pdf-view-mode)
             (memq state '(insert visual)))
        (propertize tag 'face 'evil-ex-lazy-highlight)
      tag)))
(advice-add #'evil-generate-mode-line-tag
            :around #'osf--evilize-pdf-view-highlight-states-in-mode-line)

(defun osf-evilize-pdf-view-goto-beginning ()
  (interactive)
  (let ((hscroll (window-hscroll)))
    (pdf-view-first-page)
    (image-bob)
    (image-set-window-hscroll hscroll)))

(defun osf-evilize-pdf-view-goto-page-or-end (&optional page)
  (interactive "P")
  (if page
      (pdf-view-goto-page page)
    (let ((hscroll (window-hscroll)))
      (pdf-view-last-page)
      (image-eob)
      (image-set-window-hscroll hscroll))))

(osf-evil-define-key 'normal pdf-view-mode-map
  "q" nil

  "<remap> <evil-visual-char>" #'ignore
  "<remap> <evil-visual-block>" #'ignore
  "<remap> <evil-visual-line>" #'ignore
  "<remap> <evil-insert>" #'ignore

  "j" #'pdf-view-next-line-or-next-page
  "k" #'pdf-view-previous-line-or-previous-page
  "J" #'pdf-view-next-page
  "K" #'pdf-view-previous-page
  "h" #'image-backward-hscroll
  "l" #'image-forward-hscroll
  "^" #'image-bol
  "0" #'image-bol
  "$" #'image-eol

  "C-d" #'pdf-view-scroll-up-or-next-page
  "C-u" #'pdf-view-scroll-down-or-previous-page
  "g g" #'osf-evilize-pdf-view-goto-beginning
  "G" #'osf-evilize-pdf-view-goto-page-or-end

  "C-o" #'pdf-history-backward
  "C-i" #'pdf-history-forward

  "/" #'osf-evilize-pdf-view-isearch-forward
  "?" #'osf-evilize-pdf-view-isearch-backward
  "n" #'osf-evilize-pdf-view-isearch-repeat-forward
  "N" #'osf-evilize-pdf-view-isearch-repeat-backward

  "<escape>" #'osf-evilize-pdf-view-force-normal-state

  "+" #'pdf-view-enlarge
  "-" #'pdf-view-shrink
  "=" #'pdf-view-scale-reset

  "f" #'pdf-links-isearch-link
  "F" #'pdf-links-action-perform

  "H" #'pdf-view-fit-height-to-window
  "P" #'pdf-view-fit-page-to-window
  "W" #'pdf-view-fit-width-to-window

  "g r" #'revert-buffer

  "g /" #'pdf-occur

  "<C-down-mouse-1>" #'pdf-view-mouse-extend-region
  "<M-down-mouse-1>" #'pdf-view-mouse-set-region-rectangle
  "<down-mouse-1>" #'pdf-view-mouse-set-region

  "g l" #'pdf-view-goto-label

  "'" #'pdf-view-jump-to-register
  "m" #'pdf-view-position-to-register

  "o" #'pdf-outline)

(osf-evil-define-key 'visual pdf-view-mode-map
  "j" #'pdf-view-next-line-or-next-page
  "k" #'pdf-view-previous-line-or-previous-page

  "C-d" #'pdf-view-scroll-up-or-next-page
  "C-u" #'pdf-view-scroll-down-or-previous-page)

(osf-evil-define-key 'visual pdf-view-mode-map
  "y" #'pdf-view-kill-ring-save

  "<C-down-mouse-1>" #'pdf-view-mouse-extend-region
  "<M-down-mouse-1>" #'pdf-view-mouse-set-region-rectangle
  "<down-mouse-1>" #'pdf-view-mouse-set-region)

(with-eval-after-load 'pdf-outline
  (evil-set-initial-state 'pdf-outline-buffer-mode 'normal)
  (osf-evil-define-key 'normal pdf-outline-buffer-mode-map
    "q" #'quit-window

    "RET" #'pdf-outline-follow-link-and-quit

    "S-<return>" #'pdf-outline-display-link
    "M-<return>" #'pdf-outline-follow-link

    "." #'pdf-outline-move-to-current-page

    "G" #'pdf-outline-end-of-buffer

    "^" #'pdf-outline-up-heading

    "<tab>" #'outline-toggle-children
    "<backtab>" #'pdf-outline-toggle-subtree))

(with-eval-after-load 'pdf-occur
  (evil-set-initial-state 'pdf-occur-buffer-mode 'normal)

  (osf-evil-define-key 'normal pdf-occur-buffer-mode-map
    "q" #'quit-window
    "RET" #'pdf-occur-goto-occurrence
    "S-<return>" #'pdf-occur-view-occurrence))

(provide 'osf-evilize-pdf-tools)
