(defconst negval/c-style
  '((c-tab-always-indent . t)
    (c-basic-offset . 2)
    (ident-tabs-mode . t)
    (c-offsets-alist
     (brace-list-open . +)
     (brace-list-intro . +)
     (brace-list-entry . -)
     (case-label . +)
     (inline-open . 0)
     (innamespace . 0)
     (statement-cont . +)
     (substatement-open . 0)
     (topmost-intro-cont . 0)
     )))
(defconst mfisoft/c-style
  '((c-tab-always-indent . t)
    (c-basic-offset . 4)
    (ident-tabs-mode . nil)
    (c-offsets-alist
     (brace-list-open . +)
     (case-label . +)
     (inline-open . 0)
     (innamespace . 0)
     (statement-cont . +)
     (substatement-open . 0)
     (topmost-intro-cont . 0)
     )))

(defun include-separator ()
  "include separator like //--...-"
  (interactive)
  (insert "//")
  (dotimes (i 78)
    (insert "-"))
  (insert "\n"))

(defun include-guards ()
  "include the #ifndef/#define/#endif include guards for the current buffer"
  (interactive)
  (let ((tag (concat "__"
                     (mapconcat (lambda (s) (upcase s))
                                (split-string
                                 (if (string-match "/src/" buffer-file-name)
                                     (car (reverse (split-string buffer-file-name "/src/")))
                                   '(buffer-name))
                                 "/\\|_\\|-\\|\\.") "_")  "__")))
    (insert (concat "#ifndef " tag "\n"))
    (insert (concat "#define " tag "\n\n"))
    (insert (concat "#endif /* " tag " */\n"))))

(defun mfisoft/include-guards ()
  "include the #ifndef/#define/#endif include guards for the current buffer"
  (interactive)
  (let ((tag (mapconcat (lambda (s) (upcase s))
                        (split-string
                         (if (string-match "/src/" buffer-file-name)
                             (car (reverse (split-string buffer-file-name "/src/")))
                           '(buffer-name))
                         "/\\|_\\|-\\|\\.") "_")))
    (insert (concat "#ifndef " tag "\n"))
    (insert (concat "#define " tag "\n\n"))
    (insert (concat "#endif // " tag "\n"))))

(defun include-timestamp ()
  "include timestamp"
  (interactive)
  (insert "/* Time-stamp: <> */\n"))

(defun include-gplv3 ()
  "include GPLv2 license header"
  (interactive)
  (insert
   "/*
 * Copyright (C) 2009 Denis Sukhonin <openlunatic@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */"))

(defun include-lgplv3 ()
  "include LGPLv3 license header"
  (interactive)
  (insert
   "/*
 * Copyright (C) 2009 Denis Sukhonin <openlunatic@gmail.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3
 * of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
 *
 */"))

(defun include-bsd ()
  "include modern BSD license header"
  (interactive)
  (insert
   "/*
 * Copyright (c) 2009 Denis Sukhonin <openlunatic@gmail.com>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */\n"))

(defun mfisoft/include-header ()
  "include MFI Soft's header"
  (interactive)
  (insert
   "/**
    \file
    \brief
 */\n\n"))

(defun include-header ()
  "include my header"
  (interactive)
  (insert
   "/* -*- coding: utf-8; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 2 -*- */
/*
 * Written by Denis Sukhonin <openlunatic@gmail.com>
 * 100% Public Domain - no warranty.
 *
 * $Id$
 */\n"))

;; other customizations
(defun negval/update-tagfile ()
  "try to find the top-directory of the current path, and create/update "
  "the tagfile "
  (interactive)
  (let ((old-cwd default-directory))
    (while (not (or
               (string= (expand-file-name default-directory) "/")
               (file-exists-p "configure.ac")
               (file-exists-p "configure.in")))
      (cd ".."))
    (if (not (string= (expand-file-name default-directory) "/"))
        (when (not (= 0 (call-process "gtags" nil nil nil)))
          (message "error while creating tagfile"))
      (message "no suitable directory found for tagging"))
    (cd old-cwd)))

(defun negval/c-mode-common ()
  (interactive)
  (c-add-style "negval" negval/c-style)
  (c-add-style "mfisoft" mfisoft/c-style)

  (local-set-key (kbd "M-]") 'gtags-find-tag-from-here)

  (cond
   (+work-p+ (c-set-style "mfisoft" nil))
   (t        (c-set-style "negval" nil)))

  ;; highlight some stuff; this is for _all_ c modes
  (font-lock-add-keywords nil
                          '(("\\<\\(__\\(PRETTY_\\)?FUNCTION__\\|__LINE__\\|__FILE__\\)"
                             1 font-lock-preprocessor-face prepend)))
  (setq
   compilation-scroll-output 'first-error  ; scroll until first error
   compilation-read-command nil            ; don't need enter
   compilation-window-height 16            ; keep it readable
   c-hungry-delete-key t                   ; eat as much as possible
   ;; guess the identation of the current file, and use
   c++-font-lock-extra-types (quote ("JBF[a-zA-Z0-9]*" "Q[a-zA-Z]*"
                                     "u?int\\([1-9]+_t\\)?"
                                     "\\(::\\)?\\(std\\|boost\\)::[0-9a-zA-Z:_]+")))

  ;; that instead of my own settings; nice for foreign
  ;; files
  ;; https://savannah.nongnu.org/projects/dtrt-indent/
  (when (require-maybe 'dtrt-indent) (dtrt-indent-mode t))

  (when (require-maybe 'doxymacs)
    (doxymacs-mode t)
    (doxymacs-font-lock))

  (local-set-key (kbd "C-c i s") 'include-separator)
  (local-set-key (kbd "C-c i t") 'include-timestamp)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (local-set-key (kbd "RET") 'newline-and-indent)

  ;; default .h files to C++
  (setq auto-mode-alist (append (list '("\\.h\\'" . c++-mode)) auto-mode-alist))

  (cond
   (+home-p+
    (local-set-key (kbd "C-c i h") 'include-header)
    (local-set-key (kbd "C-c i g") 'include-guards))
   (+work-p+
    (local-set-key (kbd "C-c i h") 'mfisoft/include-header)
    (local-set-key (kbd "C-c i g") 'mfisoft/include-guards)))

  ;; warn when lines are > 80 characters (in c-mode)
  (font-lock-add-keywords 'c-mode
                          '(("^[^\n]\\{80\\}\\(.*\\)$"
                             1 font-lock-warning-face prepend))))

(defun negval/c++-mode ()
  (when-available 'c-subword-mode
    (c-subword-mode))
  ;; warn when lines are > 100 characters (in c++-mode)
  (font-lock-add-keywords 'c++-mode
                          '(("^[^\n]\\{100\\}\\(.*\\)$"
                             1 font-lock-warning-face prepend)))
  (setq c-macro-cppflags
        "-DDEBUG -I ./include -x c++ `__dir=/usr/local/include; if [ -d $__dir ]; then echo -n -I$__dir; fi` -C")
  )

;; run befor all c-mode flavours
(add-hook 'c-mode-common-hook 'negval/c-mode-common)
;; run before c mode
;;(add-hook 'c-mode-hook 'negval/c-mode)
;; run before c++ mode
(add-hook 'c++-mode-hook 'negval/c++-mode)
