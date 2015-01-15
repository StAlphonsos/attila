;;; -*- mode:emacs-lisp; indent-tabs-mode:nil; tab-width:2 -*-
;;;
;;; copyright (c) 1997-2015 by attila <attila@stalphonsos.com>.
;;;                                        all rights reserved.
;;;        released under the ISC simplified BSD-style license.
;;;
;;; Time-stamp: <2015-01-13 16:05:02 attila@stalphonsos.com>
;;;
;;; Template file fu.  Pretty simple, really.  I've been using this
;;; code (or some variation on it) for almost a decade now, in every
;;; kind of EMACS.  The idea is that you can create templates for all
;;; kinds of files, strew them throughout your filesystem as apropos,
;;; and thus arrange it so that hitting C-c s (or whatever you like)
;;; in a brand new buffer will insert whatever starting point is
;;; apropriate for that kind of file in that part of your directory
;;; tree.  I have my default template files in ~/lib, and generally
;;; have a templates/ subdir at the root of every major project tree
;;; that has templates more apropriate to that specific project.
;;;

;;+
;; *template-dir* [var]
;;
;;      Default template directory
;;-
(setq *template-dir* (expand-file-name "~/lib"))

;;+
;; find-template name [func]
;;
;;    Locate a template file with the given name, starting our search
;;    in the current directory and walking up until we run out.  We
;;    check for a subdirectory called templates in each directory we look
;;    at before checking the actual directory for files.
;;-
(defun find-template (name)
  (let* ((dir "./")
         (tn (file-truename dir))
         fn)
    (while (and (null fn)
                (not (string= tn "/"))
                (not (string= tn *template-dir*)))
      (setq fn (concat tn name))
      (message "trying: %s\n" fn)
      (if (not (file-exists-p fn))
          (setq fn (concat tn "templates/" name)))
      (if (not (file-exists-p fn))
          (progn
            (setq dir (concat dir "../"))
            (setq tn (file-truename dir))
            (setq fn nil))))
    (if (null fn)
        (setq fn (concat *template-dir* "/" name)))
    (expand-file-name fn)))

;;+
;; template-file-name [func]
;;
;;     Return the filename that we should try to find for the
;;     current buffer, based on both its buffer-file-name setting
;;     and perhaps on the content of the buffer itself.  For
;;     instance, if you create a new file called foo and type
;;             #!/usr/bin/perl
;;     on its first line, template-file-name will return
;;     "template.pl", because it's obviously a perl program.
;;-
(defun template-file-name ()
  (let ((template
         (cond ((string-match "configure.in" buffer-file-name)
                "template.autoconf")
               ((string-match "Makefile.in" buffer-file-name)
                 "template.make.in")
               ((string-match ".*\\.[sS]$" buffer-file-name)
                "template.s")
               ((string-match ".*\\.txt$" buffer-file-name)
                "template.txt")
               ((string-match ".*\\.(md|mmd)$" buffer-file-name)
                "template.md")
               ((string-match ".*\\.htmpl$" buffer-file-name)
                "template.htmpl")
               ((string-match ".*\\.html$" buffer-file-name)
                "template.html")
               ((string-match ".*\\.c$" buffer-file-name)
                "template.c")
               ((string-match ".*\\.cpp$" buffer-file-name)
                "template.cpp")
               ((string-match ".*\\.C$" buffer-file-name)
                "template.cpp")
               ((string-match ".*\\.sh$" buffer-file-name)
                "template.sh")
               ((save-excursion
                  (goto-char (point-min))
                  (looking-at "#!.*sh"))
                "template.sh")
               ((string-match ".*\\.h$" buffer-file-name)
                "template.h")
               ((string-match ".*\\.js$" buffer-file-name)
                "template.js")
               ((string-match "*\\.scm$" buffer-file-name)
                "template.scm")
               ((or (string-match ".*\\.lisp$" buffer-file-name)
                    (string-match ".*\\.cl$" buffer-file-name))
               "template.lisp")
               ((or (string-match ".*\\.el$" buffer-file-name)
                    (string-match "\\.emacs$" buffer-file-name))
                "template.el")
               ((string-match "Makefile.PL" buffer-file-name)
                "template.make.PL")
               ((string-match ".*_irssi\\.pl" buffer-file-name)
                "template.irssi")
               ((string-match ".*\\.pl$" buffer-file-name)
                "template.pl")
               ((string-match ".*\\.pm$" buffer-file-name)
                "template.pm")
               ((string-match ".*\\.cgi$" buffer-file-name)
                "template.cgi")
               ((string-match ".*\\.t$" buffer-file-name)
                "template.t")
               ((or (string-match ".*\\.mason$" buffer-file-name)
                    (string-match ".*\\.mhtml$" buffer-file-name))
                "template.mason")
               ((or (string-match ".*\\.asm$" buffer-file-name)
                    (string-match ".*\\.mac$" buffer-file-name))
                "template.asm")
               ((save-excursion
                  (goto-char (point-min))
                  (looking-at "#!.*perl"))
                "template.pl")
               ((save-excursion
                  (goto-char (point-min))
                  (looking-at ".*-\*-[ ]*outline[ ]*-\*-"))
                "template.outline")
               ((or (string-match "[Mm]akefile" buffer-file-name)
                    (string-match "GNU[Mm]akefile" buffer-file-name))
               "template.make")
               ((or (string-match "Makefile\\.in" buffer-file-name))
                "template.make.in")
               ((string-match "configure\\.in" buffer-file-name)
                "template.autoconf")
               ((string-match "TODO" buffer-file-name)
                "template.outline")
               ((or (string-match ".*\\.sql$" buffer-file-name)
                    (string-match ".*\\.ddl$" buffer-file-name))
                "template.sql")
               ((string-match ".*\\.proto$" buffer-file-name)
                "template.proto")
               ((string-match ".*\\.org$" buffer-file-name)
                "template.org")
               ((string-match ".*\\.scala$" buffer-file-name)
                "template.scala")
               ((string-match ".*\\.py" buffer-file-name)
                "template.py")
               ((or (string-match "README" buffer-file-name)
                    (string-match "MANIFEST" buffer-file-name))
                "template.text")
               (t "template.unknown"))))
    (if (file-exists-p template)
        template
      (find-template template))))

;;+
;; insert-template-file [func]
;;
;;      Function meant to be called interactively or from a key
;;      binding.  Locates a template file suitable for the current
;;      buffer and inserts it at point.  Also invokes
;;      hack-local-variables after it inserts the template, to give
;;      any local variable settings in the template to take effect.
;;-
(defun insert-template-file ()
  (interactive)
  (let ((tf (template-file-name)))
    (insert-file tf)
    (hack-local-variables)))

;;+
;; C-c s [keybinding]
;;
;;      Invokes insert-template-file in the current buffer.
;;-
(global-set-key "\C-cs" 'insert-template-file)

(provide 'templates)
