;; -*- mode:emacs-lisp; indent-tabs-mode:nil -*-
;;
;; jabber-setup.el - set up my jabber fu
;;

(require 'jabber)
(setq jabber-account-list
      '(("sean.levy@jabber.internal.lytix.com" (:password . "7uIURCgX")))
      jabber-history-enabled t
      jabber-use-global-history nil
      jabber-auto-reconnect t)
(provide 'jabber-setup)
