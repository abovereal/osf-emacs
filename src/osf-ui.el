;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2026 Chen Zhexuan

;; Author: Chen Zhexuan
;; URL: https://github.com/CloseToZero/osf-emacs

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

(setq inhibit-startup-screen t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(push '(fullscreen . maximized) default-frame-alist)

(setq ring-bell-function #'ignore)

(column-number-mode)
(setq column-number-indicator-zero-based nil)

(setq display-line-numbers-grow-only t
      display-line-numbers-type 'visual)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))
(add-hook 'osf-fundamental-mode-hook #'display-line-numbers-mode)

(straight-use-package 'modus-themes)

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-slanted-constructs t
      modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui nil
      modus-themes-completions '((t . (extrabold)))
      modus-themes-prompts '(extrabold)
      modus-themes-headings
      '((agenda-structure . (variable-pitch light 2.2))
        (agenda-date . (variable-pitch regular 1.3))
        (t . (regular 1.15))))
(setq modus-themes-common-palette-overrides nil)
;; We can choose a temporary theme inside early-custom.el
(let ((theme (if (boundp 'osf-theme)
                 osf-theme
               'modus-operandi)))
  (load-theme theme t))

(provide 'osf-ui)
