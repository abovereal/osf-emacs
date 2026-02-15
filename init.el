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

(defvar osf-src-dir
  (expand-file-name "src" user-emacs-directory))

(defvar osf-cache-dir
  (expand-file-name ".cache" user-emacs-directory))

(defvar osf-system-type
  (cond ((memq system-type '(cygwin windows-nt ms-dos)) 'windows)
        ((eq system-type 'darwin) 'mac)
        ((eq system-type 'gnu/linux) 'linux)
        ((eq system-type 'berkeley-unix) 'bsd)
        (t system-type)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(push osf-src-dir load-path)
(require 'osf-core-lib)
(require 'osf-coding-system)
(require 'osf-clean-dir)
(require 'osf-package)
(require 'osf-mac)
(require 'osf-clean-mode-line)
(require 'osf-savehist)
(require 'osf-lib)
(require 'osf-evil)
(require 'osf-ui)
(require 'osf-helpful)
(require 'osf-misc)
(require 'osf-server)
(require 'osf-undo)
(require 'osf-completion)
(require 'osf-occur)
(require 'osf-todo)
(require 'osf-recent-files)
(require 'osf-smartparens)
(require 'osf-search-and-replace)
(require 'osf-window)
(require 'osf-workspace)
(require 'osf-dired)
(require 'osf-ibuffer)
(require 'osf-imenu)
(require 'osf-save-place)
(require 'osf-project)
(require 'osf-eww)
(require 'osf-scratch)
(require 'osf-vc)
(require 'osf-embark)
(require 'osf-avy)
(require 'osf-comment)
(require 'osf-shell)
(require 'osf-org)
(require 'osf-markdown)
(require 'osf-emacs-lisp)
(require 'osf-common-lisp)
(require 'osf-ml)
(require 'osf-c-cpp)
(require 'osf-rust)
(require 'osf-go)
(require 'osf-julia)
(require 'osf-tiger)
(require 'osf-zig)
(require 'osf-js-ts)
(require 'osf-eglot)
(require 'osf-dape)
(require 'osf-coq)
(require 'osf-lean)
(require 'osf-opengl)
(require 'osf-image)
(require 'osf-pdf)
(when (eq osf-system-type 'windows)
  (require 'osf-total-commander))
(require 'osf-daily)
(require 'osf-ffmpeg)
(require 'osf-enable-disabled-commands)
(require 'osf-bind-keys)

(load custom-file t t)
