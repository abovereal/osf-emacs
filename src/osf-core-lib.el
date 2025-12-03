;;; -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2025 Zhexuan Chen <2915234902@qq.com>

;; Author: Zhexuan Chen <2915234902@qq.com>
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

(require 'cl-lib)
(require 'seq)

(defun osf-regexp-literal-any-case (literal-str)
  "Returns a regexp that matches LITERAL-STR literally and case insensitively."
  (rx-to-string
   `(seq
     ,@(cl-loop
        for ch across literal-str
        collect
        (if (not (eq (upcase ch) (downcase ch)))
            `(regexp ,(rx-to-string `(any ,(char-to-string (upcase ch))
                                          ,(char-to-string (downcase ch)))))
          `(regexp ,(rx-to-string (char-to-string ch))))))))

(defun osf-ensure-is-list (x)
  (if (listp x) x (list x)))

(provide 'osf-core-lib)
