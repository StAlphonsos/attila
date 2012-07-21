;;; -*- mode:emacs-lisp; indent-tabs-mode:nil; tab-width:2 -*-
;;;
;;; Time-stamp: <2008-10-25 16:34:18 attila@stalphonsos.com>
;;;
;;; attila is a schizophrenic freak.
;;; no, he's not.
;;; hey, what are you doing here?
;;; who, me?
;;; well i don't see anyone else here!
;;; stop staring at me.
;;; we have to send that guy an email now.
;;; "we"?
;;; oh, stop.
;;;
(require 'smtpmail)
(require 'mailcrypt)
(require 'vm)

(load-library "mc-toplev")

;;
; I read multiple mail boxes, for multiple identities, all from
; my laptop.  Most of them are IMAP, but a couple are POP.  In
; all cases, I need to be able to send mail as any of those
; identities, and it's irritating to do so without some support,
; so this code is that.  So there.
;;

(setq *attila-smtp-alist*
      '((StA 10025)
        (BitsEnd 20025)
        (Cluefactory 50025)
        (SavvyIdler 50025)
        (SDF 30025)
        (DHP 40025)
        )
      )

(setq *attila-identity-alist*
      '((StA "attila <attila@stalphonsos.com>" 0)
        (BitsEnd "Sean Levy <snl@bitsend.com>" 1)
        (Cluefactory "Sean Levy <snl@cluefactory.com>" 2)
        (SavvyIdler "US 2 <us2@savvyidler.com>" 3)
        (SDF "Sean Levy <snl@sdf.lonestar.org>" 4)
        (DHP "attila <attila@dhp.com>" 5)
        )
      )

(setq *attila-current-identity* 'Cluefactory)

(defun lookup-mail-identity (addr)
  (let ((id nil))
    (if (not (null addr))
        (mapc '(lambda (i)
                 (if (string-match addr (cadr i))
                     (cond ((null id) (setq id (car i)))
                           ((listp id) (setq id (append id (list (car i)))))
                           (t (setq id (append (list id) (list (car i))))))))
              *attila-identity-alist*))
    (if (null id)
        *attila-current-identity*
      id)))

(defun find-from-address-line ()
  (let ((pt nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (< (point) (point-max))
                  (null pt)
                  (not (looking-at "^--text follows this line--")))
        (if (looking-at "^From: *\\(.*\\)$")
            (setq pt (point)))
        (forward-line 1)))
    pt))

(defun get-mail-from-address ()
  (let ((from-addr nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (< (point) (point-max))
                  (null from-addr)
                  (not (looking-at "^--text follows this line--")))
        (if (looking-at "^From: *\\(.*\\)$")
            (setq from-addr (buffer-substring (match-beginning 1)
                                              (match-end 1))))
        (forward-line 1)))
    from-addr))

(defun get-mail-identity ()
  (let* ((from-addr (get-mail-from-address))
         (idlist (lookup-mail-identity from-addr))
         (id (if (listp idlist) (car idlist) idlist)))
    (if (null id)
        (setq id *attila-current-identity*))
    id))

(defun mail-identity-set-p ()
  (if (find-from-address-line)
      t
    nil))

(defun get-smtp-server-port-for (inbox)
  (let* ((key (if (stringp inbox) (intern inbox) inbox))
         (srv (assoc key *attila-smtp-alist*)))
    (if (not (null srv))
        (cadr srv)
      25)))

(defun set-smtp-variables-for (inbox)
  (let ((port (get-smtp-server-port-for inbox)))
    (setq smtpmail-smtp-server "127.0.0.1"
          smtpmail-smtp-service port)))

(defun set-from-address (&optional id)
  (if (null id)
      (setq id *attila-current-identity*))
  (let ((pt (find-from-address-line))
        (addr (cadr (assoc id *attila-identity-alist*))))
    (save-excursion
      (cond ((null pt)
             (goto-char (point-min))
             (open-line 1))
            (t
             (goto-char pt)
             (kill-line)))
      (insert (concat "From: " addr)))))

(defun get-mail-identity-pos (id)
  (let ((rec (assoc id *attila-identity-alist*)))
    (if (null rec)
        (setq rec (assoc *attila-current-identity* *attila-identity-alist*)))
    (car (cdr (cdr rec)))))             ;elisp has no caddr? shocked, i say.

(defun next-mail-identity (id)
  (let* ((pos (get-mail-identity-pos id))
         (n (length *attila-identity-alist*))
         (newpos (% (1+ pos) n))
         (rec (nth newpos *attila-identity-alist*)))
    (car rec)))
    
(defun prev-mail-identity (id)
  (let* ((pos (get-mail-identity-pos id))
         (n (length *attila-identity-alist*))
         (newpos (if (or (zerop pos) (< 0 pos)) (1- n) (1- pos)))
         (rec (nth newpos *attila-identity-alist*)))
    (car rec)))

(defun set-mail-identity (x)
  (let ((id (if (null x) (get-mail-identity) x)))
    (if (null id)
        (setq id (caar *attila-identity-alist*)))
    (set-smtp-variables-for id)
    (set-from-address id)
    (setq *attila-current-identity* id)
    (message "[Set mail identity to: %s]" id)))

(defun attila-set-mail-identity ()
  (interactive)
  (let ((id (get-mail-identity)))
    (if (not (mail-identity-set-p))
        (set-mail-identity id)
      (set-mail-identity (next-mail-identity id)))))

(defun attila-mail-mode-hook ()
  (interactive)
  (local-set-key "\C-ci" 'attila-set-mail-identity)
  (local-set-key "\C-c\C-i" 'mail-complete)
  (message "[attila schizophrenia in effect; IDs: C-c i, Addresses: C-c Tab]"))

(add-hook 'mail-mode-hook 'attila-mail-mode-hook)

(setq mail-default-headers "FCC: ~/mail/folders/outgoing")
(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)

;;
; Mailcrypt is my buddy
;;
(mc-setversion "gpg")
(setq mc-passwd-timeout 86400
      mc-password-reader 'vm-read-password) ;my hack
(autoload 'mc-install-write-mode "mailcrypt" nil t)
(autoload 'mc-install-read-mode "mailcrypt" nil t)
(add-hook 'mail-mode-hook 'mc-install-write-mode)
(add-hook 'vm-mode-hook 'mc-install-read-mode)
(add-hook 'vm-summary-mode-hook 'mc-install-read-mode)
(add-hook 'vm-virtual-mode-hook 'mc-install-read-mode)
(add-hook 'vm-mail-mode-hook 'mc-install-write-mode)
