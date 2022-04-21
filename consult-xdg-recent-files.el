;;; consult-xdg-recent-files.el --- Open files recently used by other xdg compliant programs -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Hauke Rehfeld

;; Author: Hauke Rehfeld <emacs@haukerehfeld.de>
;; URL: https://github.com/hrehfeld/consult-xdg-recent-files
;; Version: 0.1-pre
;; Package-Requires: ((emacs "27.1"))
;; Keywords: consult xdg recent-files convenience files unix matching

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; just a slightly cleaned up version of
;; https://github.com/minad/consult/wiki#including-file-recently-used-by-other-programs
;; If you find yourself using other programs with Emacs, it can be helpful to include files used by other programs in the candidate lists of commands like consult-recent-file and consult-buffer. That way, you never have any mental hiccups when trying to open files in Emacs that you recently opened in a different program. Instead, you simply use the same interface with which you are already familiar.
;;
;; The way to access this information is generally specific to each system. Please update this section for other systems, if you find this feature useful.
;;
;; In Linux (or, more specifically, on systems that comply with the XDG specification), these files are listed in the file recently-used.xbel, which is found in the directory ~/.local/share or the location described by the environment variable XDG_DATA_HOME.
;;
;; We can access the data in this file using libraries built-in with Emacs, namely url-util.el, dom.el, and one of xml.c or xml.el.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Install these required packages:

;; + foo
;; + bar

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'consult-xdg-recent-files)

;;;; straight.el
;; (my-use-package consult-xdg-recent-files
;;   :demand (my-use-package-demand-random-time)
;;   :ensure t
;;   :straight (consult-xdg-recent-files :type git :host github :repo "hrehfeld/consult-xdg-recent-files" :protocol ssh)
;;   :config
;;   (add-to-list 'consult-buffer-sources consult-xdg-recent-files--source-system-file)
;;   )

;;;; Usage

;; Example: using the "mixed" source in `consult-buffer':
;; (setq consult-buffer-sources
;;       '( consult--source-hidden-buffer
;;          consult--source-buffer
;;          consult--source-mixed-file
;;          consult--source-bookmark
;;          consult--source-project-buffer
;;          consult--source-project-file))
;; or just
;; (add-to-list 'consult-buffer-sources consult-xdg-recent-files--source-system-file)

;;; History:
;;;; Credits
;;; Code:
;;;; Requirements

(require 'dom)
(require 'url-util)
(require 'xml)
(require 'cl-lib)

;;;; Commands

;;;; Functions

;;;;; Public

;;;;; Private

(defun consult-xdg-recent-files--xdg-recent-file-list ()
  "Get a list of recently used files on XDG-compliant systems.

This function extracts a list of files from the file
`recently-used.xbel' in the folder `xdg-data-home'.

For more information on this specification, see
https://www.freedesktop.org/wiki/Specifications/desktop-bookmark-spec/"
  (let ((data-file (expand-file-name "recently-used.xbel" (xdg-data-home)))
        (xml-parsing-func (if (libxml-available-p)
                              #'libxml-parse-xml-region
                            #'xml-parse-region)))
    (if (file-readable-p data-file)
        (delq nil
              (mapcar (lambda (bookmark-node)
                        (when-let ((local-path (string-remove-prefix
                                                "file://"
                                                (dom-attr bookmark-node 'href))))
                          (let ((full-file-name (decode-coding-string
                                                 (url-unhex-string local-path)
                                                 'utf-8)))
                            (when (file-exists-p full-file-name)
                              full-file-name))))
                      (nreverse (dom-by-tag (with-temp-buffer
                                              (insert-file-contents data-file)
                                              (funcall xml-parsing-func
                                                       (point-min)
                                                       (point-max)))
                                            'bookmark))))
      (message "consult-xdg-recent-files: List of XDG recent files not found"))))


(defun consult-xdg-recent-files--recent-system-files ()
  "Return a list of files recently used by the system."
  (cl-case system-type
    (gnu/linux
     (consult-xdg-recent-files--xdg-recent-file-list))
    (t
     (message "consult-xdg-recent-files: \"%s\" system currently unsupported"
              system-type))))

(defun consult-xdg-recent-files--recent-files-sort (file-list)
  "Sort the FILE-LIST by modification time, from most recent to least recent."
  (thread-last
    file-list
    ;; Use modification time, since getting file access time seems to count as
    ;; accessing the file, ruining future uses.
    (mapcar (lambda (f)
              (cons f (file-attribute-modification-time (file-attributes f)))))
    (seq-sort (pcase-lambda (`(,f1 . ,t1) `(,f2 . ,t2))
                ;; Want existing, most recent, local files first.
                (cond ((or (not (file-exists-p f1))
                           (file-remote-p f1))
                       nil)
                      ((or (not (file-exists-p f2))
                           (file-remote-p f2))
                       t)
                      (t (time-less-p t2 t1)))))
    (mapcar #'car)))

(defun consult-xdg-recent-files--recent-files-mixed-candidates ()
  "Return a list of files recently used by Emacs and the system.

These files are sorted by modification time, from most recent to least."
  (thread-last
    (consult-xdg-recent-files--recent-system-files)
    (seq-filter #'recentf-include-p)
    (append (mapcar #'substring-no-properties recentf-list))
    delete-dups
    (consult-xdg-recent-files--recent-files-sort)))

(defvar consult-xdg-recent-files--source-system-file
  `(:name     "System file"
              :narrow   ?F
              :category file
              :face     consult-file
              :history  file-name-history
              :action   ,#'consult--file-action
              :items
              ,(lambda ()
                 (let ((ht (consult--buffer-file-hash)))
                   (mapcar #'abbreviate-file-name
                           (seq-remove (lambda (x) (gethash x ht))
                                       (consult-xdg-recent-files--recent-system-files))))))
  "Recent system file candidate source for `consult-buffer'.")

(defvar consult-xdg-recent-files--source-mixed-file
  `(:name     "File"
              :narrow   ?f
              :category file
              :face     consult-file
              :history  file-name-history
              :action   ,#'consult--file-action
              :items
              ,(lambda ()
                 (let ((ht (consult--buffer-file-hash)))
                   (mapcar #'abbreviate-file-name
                           (seq-remove (lambda (x) (gethash x ht))
                                       (consult-xdg-recent-files--recent-files-mixed-candidates))))))
  "File candidate source for `consult-buffer', including system files.
This is meant as a replacement for `consult-xdg-recent-files--source-file'.")


;;;; Footer

(provide 'consult-xdg-recent-files)

;;; consult-xdg-recent-files.el ends here
