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

(require 'osf-evilize-minibuffer)
(require 'osf-evilize-special)
(with-eval-after-load 'button
  (require 'osf-evilize-button))
(with-eval-after-load 'help-mode
  (require 'osf-evilize-help))
(with-eval-after-load 'dired
  (require 'osf-evilize-dired))
(with-eval-after-load 'wdired
  (require 'osf-evilize-wdired))
(with-eval-after-load 'wgrep
  (require 'osf-evilize-wgrep))
(with-eval-after-load 'transient
  (require 'osf-evilize-transient))
(with-eval-after-load 'magit
  (require 'osf-evilize-magit))
(with-eval-after-load 'info
  (require 'osf-evilize-info))
(with-eval-after-load 'org
  (require 'osf-evilize-org))
(with-eval-after-load 'slime
  (require 'osf-evilize-slime))
(with-eval-after-load 'imenu-list
  (require 'osf-evilize-imenu-list))
(with-eval-after-load 'deadgrep
  (require 'osf-evilize-deadgrep))
(with-eval-after-load 'company
  (require 'osf-evilize-company))
(with-eval-after-load 'vertico
  (require 'osf-evilize-vertico))
(with-eval-after-load 'vundo
  (require 'osf-evilize-vundo))
(with-eval-after-load 'pdf-tools
  (require 'osf-evilize-pdf-tools))

(provide 'osf-evilize)
