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

(defvar osf-org-blog-file-name-time-string-format "%Y%m%d%H%M%S")

(defvar osf-org-generate-blog-file-name-hist nil)
(osf-add-saved-vars 'osf-org-generate-blog-file-name-hist)

(defvar osf-org-paste-image-from-clipboard-hist nil)
(osf-add-saved-vars 'osf-org-paste-image-from-clipboard-hist)

(setq org-startup-indented t
      org-src-tab-acts-natively t
      org-src-preserve-indentation t
      org-confirm-babel-evaluate nil
      org-element-use-cache nil
      org-capture-templates
      '(("b" "blog" plain
         (file osf-org-generate-blog-file-name)
         (function osf-blog-template)
         :hook osf-org-capture-hack-local-variables
         :after-finalize osf-org-capture-hack-local-variables-if-not-aborted
         :unnarrowed t
         :jump-to-captured t))
      org-capture-bookmark nil
      org-imenu-depth 3)

(straight-use-package 'ox-hugo)
(with-eval-after-load 'ox
  (require 'ox-hugo))
(with-eval-after-load 'ox-hugo
  (push "zip" org-hugo-external-file-extensions-allowed-for-copying))

(with-eval-after-load 'org
  (defun osf-org-export-directory-using-ox-hugo
      (directory &optional include-no-md)
    ;; include-no-md: don't export files that don't have the
    ;; corresponding markdown file.
    (interactive (list default-directory current-prefix-arg))
    (require 'ox-hugo)
    (mapc
     (lambda (file)
       (let ((opened? (get-file-buffer file)))
         (with-current-buffer (find-file-noselect file)
           (let* ((info (org-combine-plists
                         (org-export--get-export-attributes 'hugo)
                         (org-export--get-buffer-attributes)
                         (org-export-get-environment 'hugo)))
                  (pub-dir (org-hugo--get-pub-dir info))
                  (outfile (org-export-output-file-name ".md" nil pub-dir)))
             (when (or include-no-md
                       (file-exists-p outfile))
               (org-hugo-export-to-md))))
         (unless opened?
           (kill-buffer (get-file-buffer file)))))
     (directory-files directory t (rx ".org" string-end))))

  ;; Adapted from org-roam's `org-roam-node-slug'.
  (defun osf-org-normalize-file-name (file-name)
    (let ((trim-chars '(
                        ;; Combining Diacritical Marks
                        ;; https://www.unicode.org/charts/PDF/U0300.pdf
                        768 ; U+0300 COMBINING GRAVE ACCENT
                        769 ; U+0301 COMBINING ACUTE ACCENT
                        770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                        771 ; U+0303 COMBINING TILDE
                        772 ; U+0304 COMBINING MACRON
                        774 ; U+0306 COMBINING BREVE
                        775 ; U+0307 COMBINING DOT ABOVE
                        776 ; U+0308 COMBINING DIAERESIS
                        777 ; U+0309 COMBINING HOOK ABOVE
                        778 ; U+030A COMBINING RING ABOVE
                        779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                        780 ; U+030C COMBINING CARON
                        795 ; U+031B COMBINING HORN
                        803 ; U+0323 COMBINING DOT BELOW
                        804 ; U+0324 COMBINING DIAERESIS BELOW
                        805 ; U+0325 COMBINING RING BELOW
                        807 ; U+0327 COMBINING CEDILLA
                        813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                        814 ; U+032E COMBINING BREVE BELOW
                        816 ; U+0330 COMBINING TILDE BELOW
                        817 ; U+0331 COMBINING MACRON BELOW
                        )))
      (cl-flet* ((nonspacing-mark? (char) (memq char trim-chars))
                 (strip-nonspacing-marks
                   (s)
                   (string-glyph-compose
                    (apply #'string
                           (seq-remove #'nonspacing-mark?
                                       (string-glyph-decompose s)))))
                 (replace-pair (file-name pair)
                   (replace-regexp-in-string (car pair)
                                             (cdr pair) file-name)))
        (let ((pairs '(
                       ;; convert anything not alphanumeric
                       ("[^[:alnum:][:digit:]]" . "_")
                       ("__*" . "_") ; remove sequential underscores
                       ("^_" . "") ; remove starting underscore
                       ("_$" . "") ; remove ending underscore
                       )))
          (downcase (seq-reduce #'replace-pair
                                pairs (strip-nonspacing-marks file-name)))))))

  (defun osf--generate-image-filename (org-filename image-filename)
    (let ((basename (file-name-base org-filename)))
      (format (if (string-empty-p image-filename)
                  "%s-%s-image.png" "%s-%s.png")
              (substring basename (1+ (or (string-match "-" basename) -1)))
              (if (string-empty-p image-filename)
                  (format-time-string osf-org-blog-file-name-time-string-format)
                (osf-org-normalize-file-name image-filename)))))

  (defun osf-org-paste-image-from-clipboard (&optional image-filename)
    (interactive)
    (unless (derived-mode-p 'org-mode)
      (user-error "You need to be in a Org buffer"))
    (unless (buffer-file-name)
      (user-error "The Org buffer need to have an associated file for the \
command to generate a image file name"))
    (cond
     ((eq osf-system-type 'windows)
      (let ((tool (executable-find "magick")))
        (unless tool (error "Cannot find the executable magick"))
        (let ((tmp-image-filename (expand-file-name
                                   (format
                                    "%s-tmp.png"
                                    (file-name-base (buffer-file-name)))
                                   temporary-file-directory)))
          (call-process tool nil "*osf-org-paste-image-from-clipboard*" nil
                        "convert" "clipboard:" tmp-image-filename)
          (unwind-protect
              (progn
                (unless (file-exists-p tmp-image-filename)
                  (error "Create image file failed"))
                (when (called-interactively-p)
                  (setq image-filename
                        (read-string
                         "Filename of the pasted image \
(empty for generated name): "
                         nil 'osf-org-paste-image-from-clipboard-hist)))
                (let ((frame (selected-frame))
                      (image-filename (osf--generate-image-filename
                                       (buffer-file-name) image-filename)))
                  (when (and (file-exists-p image-filename)
                             (called-interactively-p)
                             (yes-or-no-p "File already exists, overwrite? "))
                    (delete-file image-filename))
                  (rename-file tmp-image-filename image-filename)
                  (insert (format "[[file:%s]]" image-filename))
                  (org-display-inline-images nil t)))
            (when (file-exists-p tmp-image-filename)
              (delete-file tmp-image-filename))))))
     (t (error "Unsupported on the system: %s" osf-system-type))))

  (osf-local-leader-define-key org-mode-map
    "e e" #'org-export-dispatch
    "e H" #'osf-org-export-directory-using-ox-hugo
    "i p" #'osf-org-paste-image-from-clipboard))

(with-eval-after-load 'org-capture
  (defun osf-org-generate-blog-file-name ()
    (let ((blog-name (read-string
                      "Blog name: " nil
                      'osf-org-generate-blog-file-name-hist))
          (math-blog? (y-or-n-p "Math blog? ")))
      (org-capture-put :osf-org-blog-name blog-name)
      (org-capture-put :osf-org-math-blog? math-blog?)
      (let ((blog-time (current-time)))
        (org-capture-put :osf-org-blog-time blog-time)
        (expand-file-name
         (format
          "%s-%s.org"
          (format-time-string
           osf-org-blog-file-name-time-string-format blog-time)
          (osf-org-normalize-file-name blog-name))
         org-directory))))

  (defvar osf-blog-template-math-blog-local-vars
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "org-math-blog-local-vars" osf-src-dir))
      (buffer-string)))
  (defun osf-blog-template ()
    "#+title: %(org-capture-get :osf-org-blog-name)\n\
#+date: %(format-time-string \"%Y-%m-%d %H:%M\" \
(org-capture-get :osf-org-blog-time))\n\
#+options: author:nil\n\
#+hugo_locale: zh\n\
#+filetags: tag1 tag2\n\
#+hugo_draft: true%?\n
%(if (org-capture-get :osf-org-math-blog?)
     osf-blog-template-math-blog-local-vars \"\")")

  (defun osf-org-capture-hack-local-variables ()
    (hack-local-variables 'no-mode))

  (defun osf-org-capture-hack-local-variables-if-not-aborted ()
    (unless org-note-abort
      (when-let* ((buffer (org-capture-get :buffer)))
        (with-current-buffer buffer
          (hack-local-variables 'no-mode)))))

  (defun osf--org-capture-delete-aborted-empty-file ()
    (when org-note-abort
      (when-let* ((buffer (org-capture-get :buffer))
                  (filename (buffer-file-name buffer)))
        (when (string-empty-p
               (with-current-buffer buffer
                 (buffer-substring-no-properties (point-min)
                                                 (point-max))))
          (kill-buffer buffer)
          (delete-file filename)
          (message "Aborted, delete empty file: %s" filename)))))

  (add-hook 'org-capture-after-finalize-hook
            #'osf--org-capture-delete-aborted-empty-file))

(straight-use-package 'org-appear)
(setq org-appear-autolinks t)
(add-hook 'org-mode-hook #'org-appear-mode)

(provide 'osf-org)
