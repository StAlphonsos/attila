;; -*- mode:emacs-lisp; indent-tabs-mode:nil; lisp-indent-offset:4 -*-
;;
;; Time-stamp: <2014-12-21 11:18:51 attila@stalphonsos.com>
;;
;; My .signature fu
;;
(setq *signature-dir* (expand-file-name "~/.sigs/"))

(defun signature-apropos (from)
    (let ((sf (concat *signature-dir* from)))
        (if (file-exists-p sf)
            sf
            (concat *signature-dir* "default"))))

(defun signature-file ()
    (let ((from (or (mail-fetch-field "From")
                    (concat (getenv "USER") "@" mail-host-address))))
        (setq from (mail-strip-quoted-names from))
        (signature-apropos from)))

(defun sign-mail-message ()
    (interactive)
    (save-excursion
        (goto-char (point-max))
        (insert "\n")
        (insert "--\n")
        (insert-file (signature-file))))

(provide 'signature-fu)
