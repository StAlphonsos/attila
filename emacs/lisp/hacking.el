;;; -*- mode:emacs-lisp; indent-tabs-mode:nil; tab-width:2 -*-
;;;
;;; Time-stamp: <2014-01-14 22:06:39 attila@stalphonsos.com>
;;;
;;; Stuff I use mainly for hacking.
;;;
(require 'templates)
(if window-system (require 'imenu))
;(require 'gtags)

(setq *attila-comment-column* 40)          ;I like my comments at col 40
(setq *attila-programming-mode-hooks*      ;these are the modes I use
      '(c-mode-hook
        c++-mode-hook
        asm-mode-hook
        assembler-mode-hook
        emacs-lisp-mode-hook
        makefile-mode-hook
        sgml-mode-hook
        html-mode-hook
        perl-mode-hook
        cperl-mode-hook
        javascript-mode-hook
        awk-mode-hook
        sh-mode-hook
        lisp-mode-hook
        scheme-mode-hook
        fundamental-mode-hook
        autoconf-mode-hook
        )
      )

;; set-assq key val list
;;
;; list is an alist.  if key is in it, then change its value to val.
;; otherwise, add key to it with the value val.
;;
(defun set-assq (key val list)
  (let ((tmp (assq key list)))
    (if tmp
        (setcdr tmp val)
      (setq list (cons (cons key val) list)))
    list))

(defun attila-c-like-mode-hook ()
  (interactive)
  (make-local-variable 'comment-leader)
  (local-set-key "," 'self-insert-command)
  (local-set-key ")" 'self-insert-command)
  (local-set-key ";" 'self-insert-command)
  (local-set-key "{" 'self-insert-command)
  (local-set-key "}" 'self-insert-command)
  (local-set-key [C-S-mouse-1] 'cscope:menu)
  ;; Hack the arglist fu to my liking
  (let* ((coa (copy-alist c-offsets-alist)))
    (setq coa (set-assq 'arglist-intro 4 coa))
    (setq coa (set-assq 'arglist-close 0 coa))
    (setq coa (set-assq 'arglist-cont 0 coa))
    (setq c-offsets-alist coa))
  (setq comment-leader " * "))

(defun attila-asm-like-mode-hook ()
  (interactive)
  (setq asm-comment-char ?\;)
  (make-local-variable 'comment-leader)
  (local-set-key (read-kbd-macro "RET") 'newline)
  (local-set-key ";" 'self-insert-command)
  (local-set-key ":" 'self-insert-command)
  (setq comment-leader ";")
  (message "[attila asm mode tweaks]")
  )

(defun attila-c++-like-mode-hook ()
  (interactive)
  (make-local-variable 'comment-leader)
  (local-set-key "," 'self-insert-command)
  (setq comment-leader "// "))

(defun attila-lisp-like-mode-hook ()
  (interactive)
  (make-local-variable 'comment-leader)
  (setq comment-leader "; "))

(defun attila-elisp-mode-hook ()
  (interactive)
  (local-set-key "]" 'self-insert-command)
  )

(defun attila-sh-like-mode-hook ()
  (interactive)
  (make-local-variable 'comment-leader)
  (setq comment-leader "# "))

(defun attila-text-like-mode-hook ()
  (interactive)
  (setq fill-column 70)
  (setq indent-tabs-mode nil)
  (if (null window-system)
      (attila-fix-kezboard))
  (local-set-key "\C-i" 'indent-for-tab-command)
  (local-set-key [(f6)] 'toggle-text-mode-auto-fill)
  (auto-fill-mode 0)
  )

(defun attila-symvar-link-value (fname)
  "Examine the attributes of the named file and return a string if the
file exists and refers to a symbolic link, and nil otherwise.  The string
we return is whatever the symbolic link points at."
  (let ((attrs (file-attributes fname)))
    (if (null attrs)
        nil
      (let ((v (car attrs)))
        (if (not (stringp v))
            nil
          v)))))

(defun attila-find-symvar-link (fname)
  "Search the directory tree starting at . and giving up when we hit
the root or our home directory, whichever comes first.  We search each
directory we traverse for a file named .fname (a dotfile named fname).
If it exists, and if it is a symbolic link, then we return whatever the
symbolic link points at.  Otherwise, we return nil."
  (let* ((dir "./")
         (tn (file-truename dir))
         (hd (file-truename (getenv "HOME")))
         val fn)
    (while (and (null fn)
                (not (string= tn "/")))
      (setq fn (concat tn "." fname))
      ;(message "Trying fn=%s" fn)
      (setq val (attila-symvar-link-value fn))
      (if (null val)
          (if (string= tn hd)
              (setq tn "/")             ;bogus
            (progn
              (setq fn nil)
              (setq dir (concat dir "../"))
              (setq tn (file-truename dir))
              ;(message "Trying tn=%s" tn)
              ))))
    val))

(defun attila-hack-buffer-local-symvar (fname vname)
  "Look for a dotfile named fname in the current directory (that is, .fname).
If it is a symbolic link, set the value of the variable named vname to whatever
the link points at, after making that variable buffer-local."
  (let ((value (attila-find-symvar-link fname)))
    (unless (boundp vname)
      (progn
        (make-local-variable vname)
        (set vname nil)))
    (if (not (null value))
        (progn
          (make-local-variable vname)
          (set vname value)
          (message "[%s: %s => %s]" (buffer-name (current-buffer)) vname value)
        )
    )
  )
)

(setq *attila-dir-symvars*
      '(user-login-name
        user-full-name
        system-name
        mail-host-name
        mail-host-address
        add-log-full-name
        add-log-mailing-address
      ))

(defun attila-hack-dir-symvars ()
  "Set all per-directory symvars that are named in
*attila-dir-symvars*.  By default these are: \\[user-login-name],
\\[user-full-name], \\[system-name], \\[mail-host-name] and
\\[mail-host-address].  We use \\[attila-hack-buffer-local-symvar] to
search for and maybe set buffer-local versions of all of those
variables based on dotfiles in your directory tree.  This allows you
to flexibly control how e.g. \\[time-stamp] works by changing what
your \\[time-stamp-format] string will produce based on the directory
tree your file lives in.  Most people don't care about this, but us
schizophrenics sure do.  Yup.  All three of us."
  (interactive)
  (mapcar
   (lambda (var)
      (attila-hack-buffer-local-symvar (symbol-name var) var))
   *attila-dir-symvars*)
  nil)

(defun attila-generic-rebufferify ()
  (interactive)
  (font-lock-fontify-buffer)
  (attila-hack-dir-symvars))

(defun attila-gtags-setup ()
  (interactive)
  (gtags-mode 1)
  (local-set-key "\M-," 'gtags-find-rtag))

(defun attila-generic-programming-mode-hook ()
  (interactive)
  (message "[attila generic programming mode hook]")
  (font-lock-mode t)
  (local-set-key [(f4)] 'attila-generic-rebufferify)
  (setq comment-column *attila-comment-column*)
  (local-set-key "\C-cs" 'insert-template-file)
  (local-set-key "\C-\M-h" 'backward-kill-word)
  (if window-system (global-set-key [s-mouse-1] 'imenu))
  ;(attila-gtags-setup)
  (attila-hack-dir-symvars))

(mapcar 
 #'(lambda (mode)
    (add-hook mode 'attila-generic-programming-mode-hook))
 *attila-programming-mode-hooks*)

(defun attila-perl-refontify ()
  (interactive)
  (font-lock-fontify-buffer)
  (attila-hack-dir-symvars)
  (normal-mode))

;; A simple convention I use: #+D marks lines in my perl code that
;; are strictly for debugging; attila-debugger-line is bound to C-c C-d,
;; and turns on/off such lines by adding a #+D to BOL, or removing one
;; if it is already there (so, I mark my debugger lines with an extra
;; #+D at EOL, too).  attila-next-debugger-line is bound to C-c C-n, and
;; finds me the next one.  This make sit simple to turn on/off debugging
;; code in small chunks.

(defun attila-debugger-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "#\\+D ")
        (progn
          (delete-char 4)
          (perl-indent-command))
      (insert "#+D "))))

(defun attila-next-debugger-line ()
  (interactive)
  (re-search-forward "#\\+D"))

(defun attila-fix-kezboard ()
  (global-set-key "\C-h" 'backward-delete-char-untabify)
  (global-set-key "\M-\C-h" 'backward-kill-word))

(defun attila-perl-mode-hook ()
  (interactive)
  (local-set-key "\C-h" 'backward-delete-char-untabify)
  (local-set-key "\M-\C-h" 'backward-kill-word)
  (local-set-key [(f4)] 'attila-perl-refontify)
  (local-set-key "\C-c\C-d" 'attila-debugger-line)
  (local-set-key "\C-c\C-n" 'attila-next-debugger-line)
  (local-set-key "," 'self-insert-command)
  (local-set-key ")" 'self-insert-command)
  (local-set-key ";" 'self-insert-command)
  (local-set-key "{" 'self-insert-command)
  (local-set-key "}" 'self-insert-command)
  (local-set-key "\C-c\C-p" 'attila-perl-podly)
  (message "attila perl mode hook"))

; N.B. time-stamp was already added to write-file-hooks by generic.el.
; This means that add-hook will put attila-hack-dir-symvars *before*
; time-stamp on the hook list, which is exactly what we want.  If it
; were the other way around, then you would not see changes right away.
(add-hook 'write-file-hooks 'attila-hack-dir-symvars)
(add-hook 'write-file-hooks 'hack-local-variables)
(add-hook 'c-mode-hook 'attila-c-like-mode-hook)
(add-hook 'c++-mode-hook 'attila-c++-like-mode-hook)
(add-hook 'asm-mode-hook 'attila-asm-like-mode-hook)
(add-hook 'assembler-mode-hook 'attila-asm-like-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'attila-lisp-like-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'attila-elisp-mode-hook)
(add-hook 'makefile-mode-hook 'attila-sh-like-mode-hook)
(add-hook 'perl-mode-hook 'attila-sh-like-mode-hook)
(add-hook 'perl-mode-hook 'attila-perl-mode-hook)
(add-hook 'cperl-mode-hook 'attila-perl-mode-hook)
(add-hook 'cperl-mode-hook 'attila-generic-programming-mode-hook)
(add-hook 'sh-mode-hook 'attila-sh-like-mode-hook)
(add-hook 'lisp-mode-hook 'attila-lisp-like-mode-hook)
(add-hook 'scheme-mode-hook 'attila-lisp-like-mode-hook)
(add-hook 'text-mode-hook 'attila-text-like-mode-hook)
(add-hook 'fundamental-mode-hook 'attila-text-like-mode-hook)
(add-hook 'html-mode-hook 'attila-text-like-mode-hook)
(add-hook 'javascript-mode-hook 'attila-generic-programming-mode-hook)

(setq cperl-auto-newline nil)

(defun perldoc (&optional what)
  (interactive "sDocument for what: ")
  (save-excursion
    (let ((name (concat "*Perldoc: " what "*"))
          (cmd (concat "perldoc -t " what)))
      (shell-command cmd name))))

;;;
;; mmm-mode: useful for Mason, mainly, and eventually WebApp
;;;
;(require 'mmm-mode)
;(setq mmm-global-mode 'maybe)
;(add-to-list 'auto-mode-alist '("\\.mhtml" . html-mode))
;(add-to-list 'auto-mode-alist '("\\(auto\\|d\\)handler" . html-mode))
;(add-to-list 'mmm-mode-ext-classes-alist '(html-mode "\\.mhtml" mason))

(add-to-list 'auto-mode-alist '("\\.proto" . protobuf-mode))
(autoload 'protobuf-mode "protobuf-mode")

;;; javascript
(autoload 'javascript-mode "javascript")
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

;;; LISP hacking

(setq inferior-lisp-program "sbcl --noinform")
;(require 'slime)
