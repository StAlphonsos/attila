;;; -*- mode:emacs-lisp; indent-tabs-mode:nil; tab-width:2 -*-
;;;
;;; Time-stamp: <2013-11-10 17:55:51 attila@stalphonsos.com>
;;;
;;; Generic EMACS setup fu, mainly globals and other random nonsense
;;;
(require 'info)
(eval-when-compile (require 'cl))

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
