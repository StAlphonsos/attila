;;; -*- mode:emacs-lisp; indent-tabs-mode:nil; tab-width:2 -*-
;;;
;;; Time-stamp: <2005-09-17 17:13:00 attila@stalphonsos.com>
;;;
;;; Random useful crap
;;;

;; Average

(defun avg (&rest nums)
  (let ((n (length nums)))
    (cond ((zerop n) 0)
          ((= 1 n) (car nums))
          (t (/ (reduce #'+ nums) n)))))

;; Sigma

(defun sigma (&rest nums)
  (let* ((n (length nums))
         (avg (if (zerop n) 0 (/ (reduce #'+ nums) n))))
    (cond ((zerop n) 0)
          ((= 1 n) (car nums))
          (t (sqrt
              (/
               (reduce #'+
                       (mapcar #'(lambda (x)
                                   (let ((q (- x avg)))
                                     (* q q)))
                               nums)
                       )
               (1- n)))))))
