;; -*- mode:emacs-lisp; indent-tabs-mode:nil; tab-width:2 -*-
;;
;; Time-stamp: <2014-01-14 22:15:56 attila@stalphonsos.com>
;;
;; Numerological fu.
;;
(require 'cl)                           ;need map, reduce

;; Cheap hack to give us something we can hang properties off of
;;
(setq *numnums* '((1 one) (2 two)   (3 three) (4 four) (5 five)
                  (6 six) (7 seven) (8 eight) (9 nine)))

;; Now record the meanings as properties
;;
(put 'one   'numerological-meaning
     "beginning, independence, determined, leader, innovative")
(put 'two   'numerological-meaning
     "harmony, cooperation, balanced, communication, sensitivity")
(put 'three 'numerological-meaning
     "creative, abundance, optimistic, imagination, gregarious")
(put 'four  'numerological-meaning
     "hard work, practical, honest, solid, traditional")
(put 'five  'numerological-meaning
     "freedom, change, learning, rebel, travel")
(put 'six   'numerological-meaning
     "family, responsiblity, romance, nurturing, honor")
(put 'seven 'numerological-meaning
     "mystery, planning, specialization, thinking, analytical")
(put 'eight 'numerological-meaning
     "authority, insight, power, money, active")
(put 'nine  'numerological-meaning
     "perfection, endings, magic, spiritual, compassion")

(defun numnum (x)
  (cadr (assoc x *numnums*)))

(defun nummean (x)
  (get (numnum x) 'numerological-meaning))

(defun explode-intstr (x)
  (map 'list '(lambda (x) (string-to-int (string x))) x))

(defun letter-to-int (x)
  (let ((ltr (upcase (aref x 0))))
    (- ltr 64)))

(defun explode-plainstr (x)
  (map 'list '(lambda (x) (letter-to-int (string x))) x))

(defun numerological-add1 (x)
  (reduce '+ (explode-intstr x)))

(defun numerological-addl (x)
  (reduce '+ (explode-plainstr x)))

(defun numerological-addstr (x)
  (let ((n (numerological-add1 x)))
    (while (> n 9)
      (setq n (numerological-add1 (int-to-string n))))
    n))

(defun numerological-meaning (n)
  (let* ((x (numerological-addstr (if (stringp n) n (int-to-string n))))
         (m (nummean x)))
    (format "%s=>%d: %s" n x m)))

(defun numerology (x)
  (interactive "nNumber: ")
  (message (numerological-meaning x)))
