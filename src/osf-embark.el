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

(straight-use-package 'embark)
(straight-use-package 'embark-consult)

(with-eval-after-load 'embark
  (embark-define-regexp-target
   git-repo-ssh-url (rx "git@" (+? anychar) ".git"))

  (add-to-list 'embark-target-finders #'embark-target-git-repo-ssh-url-at-point)

  (defun osf-embark-browser-git-repo-ssh-url (git-repo-ssh-url)
    (save-match-data
      (and (string-match (rx "git@" (group (+? anychar)) ":" (group (+? anychar)) ".git")
                         git-repo-ssh-url)
           (browse-url (concat "https://"
                               (match-string 1 git-repo-ssh-url)
                               "/"
                               (match-string 2 git-repo-ssh-url))))))

  (defvar-keymap osf-embark-git-repo-ssh-url-map
    :doc "Keymap for Embark git-repo-ssh-url actions."
    "RET" #'osf-embark-browser-git-repo-ssh-url)

  (add-to-list 'embark-keymap-alist
               '(git-repo-ssh-url osf-embark-git-repo-ssh-url-map))

  (defun osf-embark-transform-path-to-native-path (beg end)
    (interactive "r")
    (let ((new-path (osf-native-path (buffer-substring beg end))))
      (delete-region beg end)
      (insert new-path)))

  (osf-keymap-set embark-file-map
    "/" #'osf-embark-transform-path-to-native-path))

(osf-keymap-global-set
  "M-^" #'embark-act)

(provide 'osf-embark)
