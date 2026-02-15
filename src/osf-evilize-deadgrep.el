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

(evil-set-initial-state 'deadgrep-mode 'normal)
(evil-set-initial-state 'deadgrep-edit-mode 'normal)

(osf-evil-define-key 'normal deadgrep-mode-map
  "q" #'quit-window
  "RET" #'deadgrep-visit-result
  "S-<return>" #'deadgrep-visit-result-other-window
  "c s" #'deadgrep-search-term
  "t" #'deadgrep-cycle-search-type
  "f" #'deadgrep-cycle-files
  "d" #'deadgrep-directory
  "g ^" #'deadgrep-parent-directory
  "g r" #'deadgrep-restart
  "c S" #'deadgrep-incremental
  "<tab>" #'deadgrep-toggle-file-results
  "` k" #'deadgrep-kill-process
  "j" #'evil-next-visual-line
  "k" #'evil-previous-visual-line
  "C-j" #'deadgrep-forward-match
  "C-k" #'deadgrep-backward-match
  "M-j" #'deadgrep-forward-filename
  "M-k" #'deadgrep-backward-filename)

(osf-evil-define-key 'normal deadgrep-edit-mode-map
  "RET" #'deadgrep-visit-result)

(provide 'osf-evilize-deadgrep)
