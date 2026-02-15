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

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" osf-cache-dir))))

(setq auto-save-list-file-prefix
      (expand-file-name
       "auto-saves-"
       (expand-file-name
	    "auto-save-list" osf-cache-dir)))

(defvar osf-auto-save-dir
  (file-name-as-directory (expand-file-name "auto-save" osf-cache-dir)))
(unless (file-directory-p osf-auto-save-dir)
  (make-directory osf-auto-save-dir))
(setq auto-save-file-name-transforms `((".*" ,osf-auto-save-dir t)))

(setq bookmark-default-file (expand-file-name "bookmarks" osf-cache-dir))

(setq recentf-save-file (expand-file-name "recent-files" osf-cache-dir))

(setq project-list-file (expand-file-name "projects" osf-cache-dir))

(setq savehist-file (expand-file-name "saved-history" osf-cache-dir))

(setq tramp-auto-save-directory (expand-file-name "tramp/auto-save/" osf-cache-dir)
      tramp-persistency-file-name (expand-file-name "tramp/persistency.el" osf-cache-dir))

(setq org-id-locations-file
      (expand-file-name ".org-id-locations" osf-cache-dir))
(setq org-persist-directory
      (file-name-as-directory
       (expand-file-name "org-persist" osf-cache-dir)))

(setq server-auth-dir
      (file-name-as-directory
       (expand-file-name
        "server" osf-cache-dir)))

(let ((transient-cache-dir
       (expand-file-name "transient" osf-cache-dir)))
  (setq transient-levels-file
        (expand-file-name "levels.el" transient-cache-dir))
  (setq transient-values-file
        (expand-file-name "values.el" transient-cache-dir))
  (setq transient-history-file
        (expand-file-name "history.el" transient-cache-dir)))

(setq url-configuration-directory
      (file-name-as-directory
       (expand-file-name "url" osf-cache-dir)))

(setq image-dired-dir
      (file-name-as-directory
       (expand-file-name "image-dired" osf-cache-dir)))

(setq save-place-file (expand-file-name "places" osf-cache-dir))

(setq eshell-z-freq-dir-hash-table-file-name (expand-file-name "eshell-z" osf-cache-dir))

(setq lsp-server-install-dir (expand-file-name "lsp" (expand-file-name ".cache" osf-cache-dir))
      lsp-session-file (expand-file-name ".lsp-session" osf-cache-dir)
      lsp-eslint-library-choices-file (expand-file-name ".lsp-eslint-choices" osf-cache-dir))

(provide 'osf-clean-dir)
