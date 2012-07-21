;; -*- emacs-lisp -*-
;;
;; Time-stamp: <2003-07-04 13:03:10 EDT>
;;
;; My .signature fu
;;
(setq *signature-dir* (expand-file-name "~/.signatures/"))

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
