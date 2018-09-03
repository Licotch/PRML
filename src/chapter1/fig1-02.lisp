(in-package :prml.chapter1)

(ql:quickload '(:vgplot)
              :silent t)

(defun fig1-2 ()
  (let* ((x1 (vgplot:range 0 101/100 1/100))
         (y1 (map 'vector #'(lambda (x) (sin (* 2 pi x))) x1))
         (data (vgplot:load-data-file (asdf:system-relative-pathname :prml "data/data.csv"))))
    (vgplot:plot x1 y1 "g;"
                 (first data) (second data) "ob;")
    (vgplot:axis '(-0.1 1.1 -1.5 1.5))
    (vgplot:grid nil)
    (vgplot:format-plot t "set xtics 1")
    (vgplot:format-plot t "set ytics 1")
    (vgplot:xlabel "x")
    (vgplot:ylabel "t")))
