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
         (data (vgplot:load-data-file "data.csv")))
    (vgplot:format-plot t "set terminal pngcairo")
    (vgplot:format-plot t "set output 'chapter1/img/fig1-02.png'")

    (vgplot:plot x1 y1 "g"
                 (first data) (second data) "ob;")
    (vgplot:axis '(-0.1 1.1 -1.5 1.5))
    (vgplot:grid nil)
    (vgplot:format-plot t "set xtics 1")
    (vgplot:format-plot t "set ytics 1")
    (vgplot:xlabel "x")
    (vgplot:ylabel "t")

    (vgplot:format-plot t "unset output")))

(fig1-2)
