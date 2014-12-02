;;; -*- mode:emacs-lisp; indent-tabs-mode:nil; tab-width:2 -*-
;;;
;;; Time-stamp: <2014-11-27 11:40:47 attila@stalphonsos.com>
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
(require 'mu4e)
(require 'signature-fu)
(load-library "mc-toplev")

;;
; I read multiple mail boxes, for multiple identities, all from
; my laptop.  Most of them are IMAP, but a couple are POP.  In
; all cases, I need to be able to send mail as any of those
; identities, and it's irritating to do so without some support,
; so this code is that.  So there.
;;

(setq *attila-smtp-alist*
      '((StA ("smtp.i.stalphonsos.net" 25 plain "attila@stalphonsos.com" "attila"))
        (Cluefactory ("smtp.i.stalphonsos.net" 25 plain "snl@cluefactory.com" "Sean Levy"))
        (Gmail ("smtp.gmail.com" 587 starttls "cluefactory@gmail.com" "Sean Levy"))
        )
      )

(setq *attila-identity-alist*
      '((StA "attila <attila@stalphonsos.com>" 0)
        (Cluefactory "Sean Levy <snl@cluefactory.com>" 1)
        (Gmail "Sean Levy <cluefactory@gmail.com>" 2)
        )
      )

(setq *attila-current-identity* 'StA)

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

(defun get-mail-from-address (&optional msg)
  (let ((from-addr (if msg (mu4e-message-field msg :from) nil)))
    (if (null from-addr)
        (save-excursion
          (goto-char (point-min))
          (while (and (< (point) (point-max))
                      (null from-addr)
                      (not (looking-at "^--text follows this line--")))
            (if (looking-at "^From: *\\(.*\\)$")
                (setq from-addr (buffer-substring (match-beginning 1)
                                                  (match-end 1))))
            (forward-line 1))))
      from-addr))

(defun get-mail-identity (&optional msg)
  (let* ((from-addr (get-mail-from-address msg))
         (idlist (lookup-mail-identity from-addr))
         (id (if (listp idlist) (car idlist) idlist)))
    (if (null id)
        (setq id *attila-current-identity*))
    id))

(defun mail-identity-set-p ()
  (if (find-from-address-line)
      t
    nil))

(defun get-smtp-info-for (inbox)
  (let* ((key (if (stringp inbox) (intern inbox) inbox))
         (srv (assoc key *attila-smtp-alist*)))
    (if (not (null srv))
        (cadr srv)
      nil)))

(defun set-smtp-variables-for (inbox)
  (let ((vars (get-smtp-info-for inbox)))
    (setq smtpmail-smtp-server (nth 0 vars)
          smtpmail-default-smtp-server (nth 0 vars)
          smtpmail-smtp-service (nth 1 vars)
          smtpmail-stream-type (nth 2 vars)
          mu4e-reply-to-address (nth 3 vars)
          user-mail-address (nth 3 vars)
          user-full-name (nth 4 vars))))

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
    (message "Mail identity: %s" id)
    (if (not (mail-identity-set-p))
        (set-mail-identity id)
      (set-mail-identity (next-mail-identity id)))))

(defun attila-set-gmail-outbound ()
  (interactive)
  (setq smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-user "cluefactory@gmail.com"
        smtpmail-smtp-service 587))

(defun attila-set-sta-outbound ()
  (interactive)
  (setq smtpmail-stream 'plain
        smtpmail-default-smtp-server "smtp.i.stalphonsos.net"
        smtpmail-smtp-server "smtp.i.stalphonsos.net"
        smtpmail-smtp-user "attila"
        smtpmail-smtp-server 25))

(defun attila-mail-mode-hook ()
  (interactive)
  (local-set-key "\C-ci" 'attila-set-mail-identity)
  (local-set-key "\C-c\C-a" 'attila-set-sta-outbound)
  (local-set-key "\C-c\C-g" 'attila-set-gmail-outbound)
  (local-set-key "\C-c\C-i" 'mail-complete)
  (local-set-key "\C-c\C-s" 'sign-mail-message)
  (attila-set-mail-identity)
  (message "[attila schizophrenia in effect; IDs: C-c i, Addresses: C-c Tab]"))

(setq *attila-schizoid-mu4e-setup*
      '((ClueFactory ((sent "/attila@stalphonsos.com/Sent")
                      (drafts "/attila@stalphonsos.com/Drafts")
                      (trash "/attila@stalphonsos.com/Garbage")))
        (StA ((sent "/attila@stalphonsos.com/Sent")
                      (drafts "/attila@stalphonsos.com/Drafts")
                      (trash "/attila@stalphonsos.com/Garbage")))
        (Gmail ((sent "/cluefactory@gmail.com/[Gmail].Sent Mail")
                (drafts "/cluefactory@gmail.com/[Gmail].Drafts")
                (trash "/cluefactory@gmail.com/[Gmail].Trash")))))

(defun attila-mu4e-dyn-folder (msg what)
  (let* ((to (if msg (mu4e-message-field msg :to) nil))
         (from (if msg (mu4e-message-field msg :from) nil))

         (id (or (lookup-mail-identity to)
                 (lookup-mail-identity from)
                 *attila-current-identity*))
         (tbl (assoc id *attila-schizoid-mu4e-setup*)))
    (cadr (assoc what (cadr tbl)))))
         
(defun attila-mu4e-refile-folder (msg)
  "/archive")

(setq mu4e-maildir "~/Mail"
      mu4e-sent-folder (lambda (msg) (attila-mu4e-dyn-folder msg 'sent))
      mu4e-drafts-folder (lambda (msg) (attila-mu4e-dyn-folder msg 'drafts))
      mu4e-trash-folder (lambda (msg) (attila-mu4e-dyn-folder msg 'trash))
      mu4e-refile-folder (lambda (msg) (attila-mu4e-refile-folder msg))
      mu4e-user-mail-address-regexp "snl@cluefactory\.com\\|attila@stalphonsos.com\\|attila@stalphonsos\.net\\|cluefactory@gmail\.com"
      mu4e-get-mail-command "/home/attila/Mail/grind_mail.sh"
      mu4e-view-show-addresses t
      mu4e-maildir-shortcuts
      '( ("/attila@stalphonsos.com/INBOX" . ?a)
         ("/cluefactory@gmail.com/INBOX" . ?g)
         ("/openbsd" . ?b)
         ("/torbsd" . ?B)
         ("/torproject" . ?t)
         ("/nycbug" . ?n)
         )
      mu4e-reply-to-address "attila@stalphonsos.com"
      user-mail-address "attila@stalphonsos.com"
      user-full-name "attila")

(add-hook 'mail-mode-hook 'attila-mail-mode-hook)
(add-hook 'mu4e-compose-mode-hook 'attila-mail-mode-hook)
;(setq mail-default-headers "FCC: ~/Mail/outgoing")
(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)


;;
; Mailcrypt is my buddy
;;
(mc-setversion "gpg")
;(setq mc-passwd-timeout 86400
;      mc-password-reader 'vm-read-password) ;my hack
(autoload 'mc-install-write-mode "mailcrypt" nil t)
(autoload 'mc-install-read-mode "mailcrypt" nil t)
(add-hook 'mail-mode-hook 'mc-install-write-mode)
;(add-hook 'vm-mode-hook 'mc-install-read-mode)
;(add-hook 'vm-summary-mode-hook 'mc-install-read-mode)
;(add-hook 'vm-virtual-mode-hook 'mc-install-read-mode)
;(add-hook 'vm-mail-mode-hook 'mc-install-write-mode)
