;;; -*- mode:emacs-lisp;indent-tabs-mode:nil;comment-column:40;tab-width:2 -*-
;;;
;;; cblock.el - comment block fns
;;;
;;; Copyright (C) 1997-2001 by St. Alphonsos.  All Rights Reserved.
;;;
;;; Time-stamp: <2014-12-23 17:18:16 attila@stalphonsos.com>
;;; $Id: cblocks.el,v 1.1 2006/03/10 18:21:52 attila Exp $
;;;

(setq *cblock-keywords*
  '((cfile "C Source Code File"
     ((module nil "Name of module that file belongs to")
      (name nil "Name of file")
      (manpage nil "Manual page file")
      (date nil "Date of creation")
      (purpose nil "Purpose")
      (synopsis nil "Synopsis of interface" t)
      (description nil "Detailed description" t)
      (author nil "Name of author")
      (type nil "Type; One of: exported or private")
      (status nil "Status; One of: finished, experimental, unknown")
      (refs nil "See also" t)))
    (lfile "LISP Source Code File"
     ((module nil "Name of module that file belongs to")
      (name nil "Name of file")
      (date nil "Date of creation")
      (purpose nil "Purpose")
      (synopsis nil "Synopsis of interface" t)
      (description nil "Detailed description" t)
      (author nil "Name of author")
      (status nil "Status; One of: finished, experimental, unknown")
      (refs nil "See also" t)))
    (hfile "C Header File"
     ((module nil "Name of module that file belongs to")
      (name nil "Name of file")
      (manpage nil "Manual page file")
      (date nil "Date of creation")
      (purpose nil "Description of purpose of file")
      (synopsis nil "Synopsis of interface" t)
      (description nil "Detailed description" t)
      (author nil "Name of author")
      (type nil "Type; One of: exported or private")
      (status nil "Status; One of: finished, experimental, unknown")
      (refs nil "See also" t)))
    (function "C Function Interface"
        ((name n "Name of function")
         (manpage nil "Manual page file")
         (purpose p "Purpose")
         (synopsis nil "Synopsis of interface" t)
         (description d "Detailed description" t)
         (type t "Type; One of: exported, private, module")
         (status s "Status; One of: finished, experimental, unknown")
         (returns r "Return value/type" t)
         (side i "Side effects")
         (threads m "Multi-thread safety. One of: safe, unsafe, N/A")
         (author a "Name of author")
         (examples nil "Example calls" t)
         (refs nil "See also" t)))
    (macro "CPP Macro"
     ((name n "Name of macro")
      (manpage nil "Manual page file")
      (purpose p "Purpose")
      (synopsis nil "Synopsis of interface" t)
      (description d "Detailed description" t)
      (type t "Type; One of: exported, private, module")
      (status s "Status; One of: finished, experimental, unknown")
      (side i "Side effects")
      (threads m "Multi-thread safety. One of: safe, unsafe, N/A")
      (author a "Name of author")
      (examples nil "Example calls" t)
      (refs nil "See also" t)))
    (struct "C Structure, Union or Enum"
      ((name n "Name of structure/enum/union")
       (purpose p "Purpose")
       (description d "Detailed description" t)
       (type t "Type; One of: exported, private, module")
       (status s "Status; One of: finished, experimental, unknown")
       (author a "Name of author")))
    (global "Global Variable"
      ((name n "Name of global")
       (purpose p "Purpose of function")
       (description d "Detailed description" t)
       (type t "Type; One of: exported, private, module")
       (threads m "Multi-thread safety. One of: safe, unsafe, N/A")
       (author a "Name of author")))))

(setq *cblock-type-codes*
  '((c . cfile)
    (h . hfile)
    (l . lfile)
    (f . function)
    (m . macro)
    (s . struct)
    (g . global)))

(setq *cblock-style-codes*
  '((+ . verbose)
    (- . terse)
    (= . literal-html)))

(setq *cblock-default-comment-start* "/*")
(setq *cblock-default-comment-end* "*/")
(setq *cblock-default-comment-leader* " * ")
(setq *cblock-tag-regexp* "@\\(.*\\):")
(setq *cblock-verbose-end-string* "-")
(setq *cblock-default-manpage-dir* "man")

(defun cblock-style-code-regexp ()
  (concat "["
    (mapcar #'(lambda (x) (string-to-char (symbol-name (car x))))
      *cblock-style-codes*)
    "]"))

(defun cblock-type-code-regexp ()
  (upcase
   (concat "["
    (mapcar #'(lambda (x) (string-to-char (symbol-name (car x))))
      *cblock-type-codes*)
    "]")))

(defun is-white-char (char &optional newlines-p)
  (or (= 32 char)
      (= 9 char)
      (and newlines-p
     (or (= 10 char)
         (= 13 char)))))

(defun trim-trailing-spaces (str &optional newlines-p)
  (let ((len (length str))
  (i 0)
  (done nil)
  char)
    (if (zerop len)
  str
      (while (and (not done)
      (< i len))
  (setq i (1+ i))
  (setq char (string-to-char (substring str (- i))))
  (if (not (is-white-char char newlines-p))
      (setq done t)))
      (substring str 0 (1+ (- len i))))))

(defun trim-leading-spaces (str &optional newlines-p)
  (let ((len (length str))
  (i 0)
  (done nil)
  char)
    (if (zerop len)
  str
      (while (and (not done)
      (< i len))
  (setq char (string-to-char (substring str i (1+ i))))
  (if (not (is-white-char char newlines-p))
      (setq done t)
    (setq i (1+ i))))
      (substring str i len))))

(defun trim-string (str &optional newlines-p)
  (trim-trailing-spaces (trim-leading-spaces str newlines-p) newlines-p))

(defun is-long-tag (type tag)
  (let ((def (assoc type *cblock-keywords*)))
    (if def
  (let ((tdef (assoc tag (caddr def))))
    (if (> (length tdef) 3)
        (nth 3 tdef)
      nil))
      nil)))

;+F
; @Name:  parse-start-of-cblock
; @Author:  snl
; @Type:  private
; @Status:  finished
; @Purpose:  parse the start of a structured comment block
; @Returns:  a list of the style (verbose, terse, literal-html) and
;    the type (function, macro, etc.) of the comment block
; @Side:  none
; @Examples:  (parse-start-of-cblock)
;-
(defun parse-start-of-cblock (&optional comment-start-string)
  (if (null comment-start-string)
      (if (boundp 'comment-start)
    (setq comment-start-string (trim-string comment-start))
  (setq comment-start-string *cblock-default-comment-start*)))
  (if (looking-at (concat "^"
        (regexp-quote comment-start-string)
        "\\(" (cblock-style-code-regexp) "\\)"
        "\\(" (cblock-type-code-regexp)  "\\)"))
      (let ((style-code (intern (match-string 1)))
      (type-code (intern (downcase (match-string 2))))
      style type)
  (setq style (assoc style-code *cblock-style-codes*))
  (if (not (null style)) (setq style (cdr style)))
  (setq type (assoc type-code *cblock-type-codes*))
  (if (not (null type)) (setq type (cdr type)))
  (list style type))
    nil))

;+F
; @Name:  mark-cblock
; @Author:  snl
; @Type:  private
; @Status:  finished
; @Purpose:  mark the region containing the current comment block
; @Returns:  a list of start and end positions for the block
; @Side:  none
; @Examples:  (mark-cblock)
;-
(defun mark-cblock (&optional comment-start-string
            comment-leader-string
            comment-end-string)
  ;; Set up start, leader and end according to either locals or
  ;; defaults
  ;;
  (if (null comment-start-string)
      (if (boundp 'comment-start)
    (setq comment-start-string comment-start)
  (setq comment-start-string *cblock-default-comment-start*)))
  (setq comment-start-string (trim-trailing-spaces comment-start-string))
  (if (null comment-leader-string)
      (if (boundp 'comment-leader)
    (setq comment-leader-string comment-leader)
  (setq comment-leader-string *cblock-default-comment-leader*)))
  (setq comment-leader-string (trim-trailing-spaces comment-leader-string))
  (if (null comment-end-string)
      (if (boundp 'comment-end)
    (setq comment-end-string (trim-string comment-end))
  (setq comment-end-string *cblock-default-comment-end*)))
  ;; Find the start of the block. We assume we're either at it
  ;; or it is above us.
  ;;
  (let ((start-regexp (concat "^"
            (regexp-quote comment-start-string)
            "\\(" (cblock-style-code-regexp) "\\)"
            "\\(" (cblock-type-code-regexp)  "\\)"))
  start)
    (if (looking-at start-regexp)
  (setq start (point))
      (setq start (re-search-backward start-regexp (point-min) t)))
    (if start
  ;; We need to parse the start of the block so that we know if it
  ;; is verbose or not. If it is, then we're looking for the next
  ;; instance of the leader followed by -, i.e. " *-" for C,
  ;; ";-" for LISP.
  ;;
  (let ((parsed-start (parse-start-of-cblock comment-start-string))
        end
        cblock-end-str)
    (if (eq (car parsed-start) 'verbose)
        (setq cblock-end-str (concat comment-leader-string
             *cblock-verbose-end-string*))
      (if (zerop (length comment-end-string))
    ;; No end string for this style of comment and not verbose,
    ;; so just assume it ends on this line
    ;;
    (save-excursion
      (end-of-line)
      (setq end (point)))
        (setq cblock-end-str comment-end-string)))
    (if (null end)
        (setq end (re-search-forward (regexp-quote cblock-end-str)
             (point-max) t)))
    (if end
        (progn
    (push-mark end t)
    (goto-char start)
    (exchange-point-and-mark)
    (list start end))
      nil))
      nil)))

;+F
; @Name:  locate-next-cblock-tag
; @Author:  snl
; @Type:  private
; @Status:  finished
; @Purpose:  locate the next tag in the currently marked cblock
; @Returns:  a list of the name of the tag and the start and end positions
; @Side:  none
; @Examples:  (locate-next-cblock-tag)
;-
(defun locate-next-cblock-tag (&optional comment-leader-string
                                         comment-end-string)
  (if (null comment-leader-string)
      (if (boundp 'comment-leader)
    (setq comment-leader-string comment-leader)
  (setq comment-leader-string *cblock-default-comment-leader*)))
  (setq comment-leader-string (trim-trailing-spaces comment-leader-string))
  (if (null comment-end-string)
      (if (boundp 'comment-end)
    (setq comment-end-string (trim-string comment-end))
  (setq comment-end-string *cblock-default-comment-end*)))
  (let ((null-end (zerop (length comment-end-string)))
  (nxt (re-search-forward *cblock-tag-regexp* (point-max) t)))
    (if nxt
  (let ((tag (intern (downcase (match-string 1)))))
    (if (re-search-forward *cblock-tag-regexp* (point-max) t)
        (progn
    (beginning-of-line)
    (list tag nxt (point)))
      (if (or (re-search-forward (regexp-quote
          (concat comment-leader-string
            *cblock-verbose-end-string*))
               (point-max) t)
        (and (not null-end)
       (re-search-forward (regexp-quote comment-end-string)
              (point-max) t)))
    (progn
      (beginning-of-line)
      (list tag nxt (point)))
        nil)))
      nil)))

(defun extract-cblock-tag-text (info tagpos &optional comment-leader-string
                              comment-end-string)
  (if (null comment-leader-string)
      (if (boundp 'comment-leader)
    (setq comment-leader-string comment-leader)
  (setq comment-leader-string *cblock-default-comment-leader*)))
  (if (null comment-end-string)
      (if (boundp 'comment-end)
    (setq comment-end-string comment-end)
  (setq comment-end-string *cblock-default-comment-end*)))
  (save-excursion
    (let ((cb-type (cadr info))
    (tag-name (car tagpos))
    (tag-start (cadr tagpos))
    (tag-end (caddr tagpos))
    (text "")
    (leader-length (length comment-leader-string))
    (end-length (length comment-end-string))
    (alt-end-str (regexp-quote
      (concat (trim-trailing-spaces comment-leader-string)
        *cblock-verbose-end-string*)))
    pos)
      (goto-char tag-start)
      (setq pos tag-start)
      (while (and (<= pos tag-end)
      (< pos (point-max)))
  (beginning-of-line 2)
  (setq text (concat text (buffer-substring pos
              (if (is-long-tag cb-type
                   tag-name)
                  (point)
                (1- (point))))))
  (if (looking-at alt-end-str)
      (setq pos (1+ (point-max)))
         (if (and (not (zerop end-length))
       (looking-at (regexp-quote comment-end-string)))
        (setq pos (1+ (point-max)))
      (if (looking-at (regexp-quote comment-leader-string))
    (progn
      (setq pos (+ (point) leader-length))
      (goto-char pos)
      (if (and (is-long-tag cb-type tag-name)
         (looking-at " "))
          (progn
      (setq pos (1+ pos))
      (goto-char pos)))
      )))))
      (trim-string text (not (is-long-tag cb-type tag-name))))))

;+F
; @Name:  parse-cblock
; @Author:  snl
; @Type:  private
; @Status:  finished
; @Purpose:  parse a cblock
; @Returns:  a list whose car is a list describing the cblock and whose
;    other elements are lists whose cars are tags and whose
;    cadrs are the strings for those tags
; @Side:  none
; @Examples:  (locate-next-cblock-tag)
;-
(defun parse-cblock (&optional comment-start-string
             comment-leader-string
             comment-end-string)
  (let ((cblock-extents (mark-cblock comment-start-string
             comment-leader-string
             comment-end-string))
        start-info
        next-tag
        parsed
        text)
    (if cblock-extents
        (save-excursion
          (narrow-to-region (car cblock-extents) (cadr cblock-extents))
          (goto-char (point-min))
          (setq start-info (parse-start-of-cblock comment-start-string))
          (setq parsed (list start-info))
          (setq next-tag (locate-next-cblock-tag comment-leader-string
                                                 comment-end-string))
          (while (not (null next-tag))
            (setq text (extract-cblock-tag-text start-info next-tag
                                                comment-leader-string
                                                comment-end-string))
            (setq parsed (append parsed (list (list (car next-tag) text))))
            (setq next-tag (locate-next-cblock-tag comment-leader-string
                                                   comment-end-string)))
          (widen)
          parsed))))

(defun find-next-cblock (&optional comment-start-string)
  (if (null comment-start-string)
      (if (boundp 'comment-start)
    (setq comment-start-string (trim-string comment-start))
  (setq comment-start-string *cblock-default-comment-start*)))
  (let ((start-regexp (concat "^"
            (regexp-quote comment-start-string)
            "\\(" (cblock-style-code-regexp) "\\)"
            "\\(" (cblock-type-code-regexp)  "\\)"))
  start)
    (if (looking-at start-regexp)
  (setq start (point))
      (setq start (re-search-forward start-regexp (point-max) t)))
    start))

(defun parse-buffer-cblocks (&optional comment-start-string
               comment-leader-string
               comment-end-string)
  (save-excursion
    (goto-char (point-min))
    (let ((nxt (find-next-cblock comment-start-string))
    parsed-cblocks
    cblock)
      (while (not (null nxt))
  (setq cblock (parse-cblock comment-start-string
           comment-leader-string
           comment-end-string))
  (setq parsed-cblocks (if (null parsed-cblocks)
         (list cblock)
             (append parsed-cblocks (list cblock))))
  (setq nxt (find-next-cblock comment-start-string)))
      parsed-cblocks)))

(defun prompt-for-cblock (type &optional style)
  (let* ((tags (caddr (assoc type *cblock-keywords*)))
   (n-tags (length tags))
   (i 0)
   cblock)
    (setq style 'verbose)    ;xxx not an option at the moment
    (setq cblock (list (list style type)))
    (while (< i n-tags)
      (let* ((tag (nth i tags))
       (val (read-from-minibuffer (concat (nth 2 tag) ": "))))
  (setq cblock (append cblock (list (list (nth 0 tag) val))))
  (setq i (1+ i))))
    cblock))

(defun insert-template-cblock (type &optional comment-start-string
                      comment-leader-string
                comment-end-string)
  (interactive "SType (function, macro, ...): ")
  (if (null comment-start-string)
      (if (boundp 'comment-start)
    (setq comment-start-string comment-start)
  (setq comment-start-string *cblock-default-comment-start*)))
  (if (null comment-leader-string)
      (if (boundp 'comment-leader)
    (setq comment-leader-string comment-leader)
  (setq comment-leader-string *cblock-default-comment-leader*)))
  (if (null comment-end-string)
      (if (boundp 'comment-end)
    (setq comment-end-string comment-end)
  (setq comment-end-string *cblock-default-comment-end*)))
  (let* ((tags (caddr (assoc type *cblock-keywords*)))
   (n-tags (length tags))
   (i 0)
   cblock)
    (setq cblock (list (list 'verbose type)))
    (while (< i n-tags)
      (let* ((tag (nth i tags)))
  (setq cblock (append cblock (list (list (nth 0 tag)
            (concat "--" (nth 2 tag) "--"
              ))))))
      (setq i (1+ i)))
    (insert-cblock cblock comment-start-string comment-leader-string
              comment-end-string)))

(defun insert-filled-string-with-tag (tag string &optional
                    comment-leader-string)
  (if (null comment-leader-string)
      (if (boundp 'comment-leader)
    (setq comment-leader-string comment-leader)
  (setq comment-leader-string *cblock-default-comment-leader*)))
  (let ((old-fill-prefix fill-prefix)
  (here (point))
  (tag-string (concat "@" (capitalize (symbol-name tag)) ":"))
  there)
    (setq fill-prefix (concat comment-leader-string "\t\t"))
    (insert (concat fill-prefix string))
    (setq there (point))
    (fill-region-as-paragraph here there)
    (setq there (point))
    (goto-char here)
    (forward-char (length comment-leader-string))
    (delete-char 1)
    (insert tag-string)
    (setq fill-prefix old-fill-prefix)
    (goto-char (+ (1- there) (length tag-string)))))

(defun insert-cblock (cblock &optional comment-start-string
                     comment-leader-string
               comment-end-string)
  (if (null comment-start-string)
      (if (boundp 'comment-start)
    (setq comment-start-string (trim-string comment-start))
  (setq comment-start-string *cblock-default-comment-start*)))
  (if (null comment-end-string)
      (if (boundp 'comment-end)
    (setq comment-end-string comment-end)
  (setq comment-end-string *cblock-default-comment-end*)))
  (if (null comment-leader-string)
      (if (boundp 'comment-leader)
    (setq comment-leader-string comment-leader)
  (setq comment-leader-string *cblock-default-comment-leader*)))
  (let ((i 1)
  (cblock-length (length cblock))
  (style (car (rassoc (caar cblock) *cblock-style-codes*)))
  (type (car (rassoc (cadar cblock) *cblock-type-codes*)))
  )
    (insert (concat (trim-trailing-spaces comment-start-string)
        (symbol-name style)
        (upcase (symbol-name type)) "\n"))
    (while (< i cblock-length)
      (let ((tag (nth i cblock)))
  (insert-filled-string-with-tag (car tag) (cadr tag)
               comment-leader-string)
  (setq i (1+ i))))
    (insert (concat (trim-trailing-spaces comment-leader-string)
        *cblock-verbose-end-string* "\n"))
    (if (not (zerop (length comment-end-string)))
  (insert (concat comment-end-string "\n")))))

(defun cblock-html (cblock &optional level)
  (let* ((desc (car cblock))
   (name (cadr (assoc 'name cblock)))
   (i 1)
   (n (length cblock))
  (result ""))
    (if (null level) (setq level 2))
    (setq result (concat "<H" (int-to-string level) ">"
       (upcase (symbol-name (cadr desc)))
       ": "
       name
       "</H" (int-to-string level) ">\n"))
    (while (< i n)
      (let ((tag (car (nth i cblock)))
      (val (cadr (nth i cblock))))
  (if (not (eq tag 'name))
      (setq result (concat result
         "<B>" (capitalize (symbol-name tag)) "</B>: "
         val "<BR>\n")))
  (setq i (1+ i))))
    result))

;;This is totally bogus.
;
;(defun generate-html-cblock-buffer (cblocks &optional name)
;  (if (null name)
;      (setq name (concat buffer-file-name ".html")))
;  (let ((b (create-file-buffer name))
;  (nm buffer-file-name)
;  (i 0)
;  (n (length cblocks)))
;    (save-excursion
;      (set-buffer b)
;      (mark-whole-buffer)
;      (kill-region (point) (mark))
;      (insert (concat "<H1>" nm "</H1>\n"))
;      (insert (concat "Automatically generated documentation for file: "
;          nm "\n<P>\n"))
;      (while (< i n)
;  (insert (cblock-html (nth i cblocks)))
;  (insert "<P>\n")
;  (setq i (1+ i)))
;      (insert "<P>\n<HR>\n")
;      name)))

;+F
; @Name:  generate-cblock-man-page-entry-string
; @Purpose:  generate a man page entry from a cblock
; @Synopsis:  (generate-cblock-man-page-entry-string cblock)
; @Description:  turns a cblock into a string containing a man page entry
;     suitable for saving as a file and processing with the
;     man(1) command.
; @Type:  exported
; @Status:  finished
; @Returns:  a string containing the man page entry
; @Side:  none
; @Threads:  N/A
; @Author:  Sean Levy <attila@dhp.com>
; @Examples:  (generate-cblock-man-page-entry-string (prompt-for-cblock))
;-
(defun generate-cblock-man-page-entry-string (cblock)
  (let ((result ".\\\" (C) 1997-2001 by St. Alphonsos. All Rights Reserved.\n")
  (name (cadr (assoc 'name cblock)))
  (author (cadr (assoc 'author cblock)))
  (purpose (cadr (assoc 'purpose cblock)))
  (syn (assoc 'synopsis cblock))
  (desc (assoc 'description cblock))
  (side (assoc 'side cblock))
  (ret (assoc 'returns cblock))
  (ex (assoc 'examples cblock))
  (threads (assoc 'threads cblock))
  (refs (assoc 'refs cblock)))
    (setq result (concat result
       ".TH " name " 3 \"" (current-time-string)
       "\" \"St. Alphonsos\" \"StAPM\"\n"
       ".SH NAME\n"
       name " \- " purpose "\n"
       ".SH SYNOPSIS\n"
       (if syn
           (concat ".nf\n" (cadr syn) "\n.fi")
         "None given.") "\n"
       ".SH DESCRIPTION\n"
       (if desc (cadr desc) "None given.") "\n"
       (if side (concat ".SH \"SIDE EFFECTS\"\n"
            (cadr side) "\n")
         "")
       (if ret (concat ".SH \"RETURN VALUES\"\n"
           (cadr ret) "\n")
         "")
       (if threads (concat ".SH THREADS\n"
               (cadr threads) "\n")
         "")
       (if ex
           (concat ".SH EXAMPLES\n.nf\n"
             (cadr ex) "\n.fi\n")
         "")
       ".SH AUTHOR\n" author "\n"
       (if refs
           (concat ".SH \"SEE ALSO\"\n" (cadr refs) "\n")
         "")
       ))
    result))

(defun create-cblock-man-page (filename)
  (interactive "FMan page file name: ")
  (save-excursion
    (let ((cblock (parse-cblock))
    buffer)
      (setq buffer (create-file-buffer filename))
      (set-buffer buffer)
      (insert (generate-cblock-man-page-entry-string cblock))
      (write-file filename t))))

(defun auto-create-cblock-man-page (cblock)
  (let ((manpage (assoc 'manpage cblock)))
    (if (null manpage)
  (setq manpage (concat *cblock-default-manpage-dir* "/"
            (cadr (assoc 'name cblock)) ".3"))
      (setq manpage (cadr manpage)))
    (let ((buffer (create-file-buffer manpage)))
      (save-excursion
  (set-buffer buffer)
  (insert (generate-cblock-man-page-entry-string cblock))
  (write-file manpage nil))
      (kill-buffer buffer)
      manpage)))

(defun auto-create-buffer-man-pages (&optional comment-start-string
                 comment-leader-string
                 comment-end-string)
  (interactive)
  (let* ((cblocks (parse-buffer-cblocks comment-start-string
          comment-leader-string
          comment-end-string))
   (n (length cblocks))
   (i 0))
    (message "Writing %d man pages..." n)
    (while (< i n)
      (auto-create-cblock-man-page (nth i cblocks))
      (setq i (1+ i)))
    (message "Writing %d man pages... done." n)))
