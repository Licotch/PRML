(ql:quickload '(:vgplot
                :alexandria)
              :silent t)

(in-package :cl-user)
(defpackage :prml.chapter1
  (:use :cl)
  (:export :fig1-02))
(in-package :prml.chapter1)

(defun fig1-2 ()
  (let* ((x1 (vgplot:range 0 101/100 1/100))
         (y1 (map 'vector #'(lambda (x) (sin (* 2 pi x))) x1))
         (x2 (vgplot:range 0 10/9 1/9))
         (y2 (map 'vector #'(lambda (x) (+ (alexandria:gaussian-random -0.3 0.3)
                                           (sin (* 2 pi x))))
                  x2)))
    (vgplot:plot x1 y1 ""
                 x2 y2 "og;")
    (vgplot:axis '(-0.1 1.1 -1.5 1.5))
    (vgplot:grid nil)
    (vgplot:format-plot t "set xtics 1")
    (vgplot:format-plot t "set ytics 1")
    (vgplot:xlabel "x")
    (vgplot:ylabel "t")
    (vgplot:print-plot #P"chapter1/img/fig1-02.png")
    (sleep 3)
    (vgplot:close-all-plots)))

(fig1-2)
