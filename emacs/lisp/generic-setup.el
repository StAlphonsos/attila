;;; -*- mode:emacs-lisp; indent-tabs-mode:nil; tab-width:2 -*-
;;;
;;; Time-stamp: <2014-05-06 16:37:24 attila@stalphonsos.com>
;;;
;;; Generic EMACS setup fu, mainly globals and other random nonsense
;;; Pulled in by ~/.emacs on Unix systems.
;;;
(require 'info)
(eval-when-compile (require 'cl))

(setenv "IN_EMACS" "t")
(put 'narrow-to-region 'disabled nil) ;I don't think this is current anymore
(put 'eval-expression 'disabled nil)    ;  nor this, but what the hell
(put 'narrow-to-page 'disabled nil)     ;narrow-to-page rules
(put 'set-goal-column 'disabled t) ;... but We do not like set-goal-column
(setq visible-bell t)
; c.f. hacking.el for more time-stamp related fu
(autoload 'time-stamp "time-stamp" "Update the time stamp in a buffer" t)
(if (not (memq 'time-stamp write-file-hooks))
    (setq write-file-hooks
          (cons 'time-stamp write-file-hooks)))
(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %u@%h")
(global-set-key "\M-g" 'goto-line)        ;Much better than the def in GNU
(global-set-key "\C-\M-h" 'backward-kill-word) ;always
(global-set-key [M-up] 'text-scale-increase)
(global-set-key [M-down] 'text-scale-decrease)
(if (not (null window-system))
    (setq scroll-step 1))                ;Makes sense in a windowing env
(setq mail-host-name "stalphonsos.com")
(setq mail-host-address "stalphonsos.com")
(setq user-mail-address "attila@stalphonsos.com")
(setq shell-multiple-shells nil)
(setq frame-title-format
      (format "[EMACS] %s@%s: %%b" (getenv "LOGNAME") (getenv "NODENAME")))
(defun insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))
(global-set-key "\C-cd" 'insert-date)        ;I've grown accustomed to this
;(load-library "ispell")
;(setq ispell-program-name "ispell")
;(setq font-lock-support-mode 'fast-lock-mode) ;Any other alternative sucks
(global-font-lock-mode 1)
(setq Info-directory-list (append (list (expand-file-name "~/emacs/info"))
                                  Info-default-directory-list))
(setq diary-file "~/personal/diary")
;(setq explicit-shell-file-name "/usr/bin/zsh"
;      explicit-zsh-args '("-l"))
;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(78 50))
(add-to-list 'default-frame-alist '(alpha 78 50))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(78 50))))
(global-set-key (kbd "C-c t") 'toggle-transparency)
;; A keyboard macro to reset EMACS' SSH_AUTH_SOCK envar from the value
;; stashed in ~/.ssh/agent_info by my zsh ssh fu.  You have to run it
;; once before you try to ssh anywhere via M-x ssh
(fset 'reset-ssh-auth-sock
   [?\C-x ?2 ?\C-x ?\C-f ?~ ?/ ?. ?s ?s ?h ?/ ?a ?g ?e ?n ?t ?_ ?i ?n ?f ?o return ?\C-  ?\C-s ?= ?\C-m ?\C-b ?\M-w ?\C-x ?o ?\( ?s ?e ?t ?e ?n ?v ?  ?\" ?\C-y ?\" ?  ?\C-x ?o ?\C-f ?\C-  ?\C-e ?\M-w ?\C-x ?o ?\" ?\C-y ?\" ?\) ?\C-j ?\C-x ?o ?\C-x ?k return ?\C-x ?1])
