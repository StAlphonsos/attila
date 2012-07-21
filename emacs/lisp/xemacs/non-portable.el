;;; -*- mode:emacs-lisp; indent-tabs-mode:nil; tab-width:2 -*-
;;;
;;; Time-stamp: <2006-03-04 14:12:48 attila@stalphonsos.com>
;;;
;;; Non-portable hacks
;;;

;; override functions that need it for our skuldudgery

(setq *old-user-login-name* (symbol-function 'user-login-name)
      *old-system-name* (symbol-function 'system-name))

(defun user-login-name (&optional ignore)
  (if (boundp 'user-login-name)
      (symbol-value 'user-login-name)
    (if (boundp 'user-name)
        (symbol-value 'user-name)
      (funcall *old-user-login-name* ignore))))

(defun system-name ()
  (if (boundp 'system-name)
      (symbol-value 'system-name)
    (funcall *old-system-name*)))

(provide 'non-portable)
