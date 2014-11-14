;;; -*- mode:emacs-lisp; indent-tabs-mode:nil; tab-width:2 -*-
;;;
;;; Time-stamp: <2014-11-12 15:40:42 attila@stalphonsos.com>
;;;
;;; Stuff I use while hacking words
;;;

;; Writing support code.

(defun count-sentences-in-paragraph (&optional no-msgs-p)
  (interactive)
  (let ((eop nil)
        (count 0))
    (save-excursion
      (if (< (point) (point-max))
          (progn
            (mark-paragraph)
            (narrow-to-region (point) (mark))
            (while (and (looking-at "$") (< (point) (point-max)))
              (beginning-of-line)
              (forward-line 1))
            (while (not (looking-at "$"))
              (setq count (1+ count))
              (forward-sentence 1))
            (widen)
            (while (and (looking-at "$") (< (point) (point-max)))
              (beginning-of-line)
              (forward-line 1))
            (setq eop (point))
            (message "There are %d sentences in this paragraph." count))))
    (if eop (goto-char eop))
    count))

(defun average-sentences-per-paragraph ()
  (interactive)
  (let ((count 0)
        (paragraphs 0)
        (avg 0.0))
    (save-excursion
      (goto-char (point-min))
      (setq count (count-sentences-in-paragraph))
      (while (not (zerop count))
        (setq paragraphs (1+ paragraphs))
        (setq count (+ count (count-sentences-in-paragraph))))
      (message
        "There are %.1f sentence/paragraphs in this buffer (%d in %d paras)"
        (/ (float count) (float paragraphs))
        count paragraphs))))

(defun words-in-sentence (&optional no-msgs-p)
  (interactive)
  (let ((eos nil)
        (count 0))
    (save-excursion
      (backward-sentence)
      (let ((sos (point)))
        (forward-sentence 1)
        (setq eos (point))
        (if (> eos sos)
            (progn
              (setq count 1)
              (narrow-to-region sos eos)
              (goto-char (point-min))
              (while (forward-word 1)
                (setq count (1+ count)))
              (widen)))))
    (if (not no-msgs-p)
        (message "There are %d words in this sentence." count))
    count))

(defun words-in-paragraph (&optional no-msgs-p)
  (interactive)
  (let ((count 0))
    (save-excursion
      (if (< (point) (point-max))
          (progn
            (mark-paragraph)
            (narrow-to-region (point) (mark))
            (goto-char (point-min))
            (setq count 1)
            (while (forward-word 1)
              (setq count (1+ count)))
            (widen)
            )))
    (unless no-msgs-p
      (message "There are %d words in this paragraph." count))
    count)
  )

(defun attila-insert-tilde-n ()
  (interactive)
  (insert 241))

(defun attila-writing-modes-hook ()
  (setq fill-column 70)
  (auto-fill-mode 1)
  (local-set-key "\M-&" 'count-sentences-in-paragraph)
  (local-set-key "\M-*" 'average-sentences-per-paragraph)
  (local-set-key "\M-^" 'words-in-sentence)
  (local-set-key "\M-n" 'attila-insert-tilde-n)
  )

(add-hook 'text-mode-hook 'attila-writing-modes-hook)
(add-hook 'fundamental-mode-hook 'attila-writing-modes-hook)
(add-hook 'mail-mode-hook 'attila-writing-modes-hook)
(set-default 'fill-column 70)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
