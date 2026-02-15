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

(require 'widget)

(defun osf-generate-section-task-list ()
  (interactive)
  (pop-to-buffer "*osf-generate-section-task-list*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (let ((w-title nil)
        (w-summary nil)
        (w-section-begin-num nil)
        (w-section-end-num nil)
        (w-generate nil)
        (w-result nil))
    (setq w-title
          (widget-create 'editable-field
                         :size 13
                         :format "标题: %v"
                         ""))
    (widget-insert "\n章节 - ")
    (setq w-section-begin-num
          (widget-create 'editable-field
                         :size 13
                         :format "起始: %v"
                         "1.1"))
    (widget-insert " ")
    (setq w-section-end-num
          (widget-create 'editable-field
                         :size 13
                         :format "结束: %v"
                         "1.7"))
    (widget-insert "\n总览: ")
    (setq w-summary
          (widget-create 'checkbox nil))
    (widget-insert "\n\n")
    (setq w-generate
          (widget-create
           'push-button
           :notify
           (lambda (&rest ignore)
             (let* ((title (widget-value w-title))
                    (section-nums-begin
                     (split-string (widget-value w-section-begin-num) "\\."))
                    (section-nums-end
                     (split-string (widget-value w-section-end-num) "\\."))
                    (chapters
                     (let* ((prefix (string-join (butlast section-nums-begin) "."))
                            (result (string-join
                                     (cl-loop for i
                                              from (string-to-number (car (last section-nums-begin)) 10)
                                              to (string-to-number (car (last section-nums-end)) 10)
                                              collect (format "%s %s.%d" title prefix i))
                                     "\n")))
                       (if (widget-value w-summary)
                           (concat (format "%s ch%s 前言\n" title (nth 0 section-nums-begin))
                                   result)
                         result))))
               (widget-value-set w-result chapters)
               (goto-char (widget-field-start w-result))
               (kill-new chapters)
               (message "Result saved into the kill ring")))
           "生成"))
    (widget-insert "\n\n")
    (setq w-result
          (widget-create 'text
                         :size 13
                         :format "结果:\n%v"
                         ""))
    (widget-insert "\n")
    (osf-evil-define-key '(insert normal) 'local
      "<tab>" #'widget-forward
      "<backtab>" #'widget-backward
      "g RET" (lambda ()
                (interactive)
                (goto-char (osf-widget-button-start w-generate))))
    (widget-setup)
    (goto-char (widget-field-start w-title))))

(defun osf-generate-chapter-task-list ()
  (interactive)
  (pop-to-buffer "*osf-generate-chapter-task-list*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (let ((w-chapter-begin-num nil)
        (w-chapter-end-num nil)
        (w-generate nil)
        (w-result nil))
    (widget-insert "章 - ")
    (setq w-chapter-begin-num
          (widget-create 'editable-field
                         :size 13
                         :format "起始: %v"
                         "1"))
    (widget-insert " ")
    (setq w-chapter-end-num
          (widget-create 'editable-field
                         :size 13
                         :format "结束: %v"
                         "7"))
    (widget-insert "\n\n")
    (setq w-generate
          (widget-create
           'push-button
           :notify
           (lambda (&rest ignore)
             (let* ((chapter-num-begin
                     (string-to-number (widget-value w-chapter-begin-num) 10))
                    (chapter-num-end
                     (string-to-number (widget-value w-chapter-end-num) 10))
                    (chapters
                     (string-join
                      (cl-loop for i from chapter-num-begin to chapter-num-end
                               collect (format "ch%s" i))
                      "\n")))
               (widget-value-set w-result chapters)
               (goto-char (widget-field-start w-result))
               (kill-new chapters)
               (message "Result saved into the kill ring")))
           "生成"))
    (widget-insert "\n\n")
    (setq w-result
          (widget-create 'text
                         :size 13
                         :format "结果:\n%v"
                         ""))
    (widget-insert "\n")
    (osf-evil-define-key '(insert normal) 'local
      "<tab>" #'widget-forward
      "<backtab>" #'widget-backward
      "g RET" (lambda ()
                (interactive)
                (goto-char (osf-widget-button-start w-generate))))
    (widget-setup)
    (goto-char (widget-field-start w-chapter-begin-num))))

(defun osf-widget-button-start (widget)
  (let ((overlay (widget-get widget :button-overlay)))
    (if (overlayp overlay)
	    (overlay-start overlay)
      (car overlay))))

(provide 'osf-daily)
