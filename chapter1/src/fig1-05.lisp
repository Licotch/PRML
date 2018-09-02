(ql:quickload '(:vgplot)
              :silent t)
(load "utils/util")

(in-package :cl-user)
(defpackage :prml.chapter1
  (:use :cl)
  (:export :fig1-5))
(in-package :prml.chapter1)

(defun e (x y w)
  (* 1/2 (loop :for n :below (length x)
               :summing (expt (- (aref (util:estimated-y x w) n) (aref y n)) 2))))

(defun erms (x y w)
  (sqrt (/ (* 2 (e x y w)) (length x))))

(defun fig1-5 ()
  (let* ((m (vgplot:range 0 10))
         (training-data (vgplot:load-data-file "data.csv"))
         (training-x (first training-data))
         (training-y (second training-data))
         (training-w (make-array 10
                                 :initial-contents
                                 (loop :for m :across m
                                       :collect (util:minimized-w training-x training-y m))))
         (training-erms (make-array 10
                                    :initial-contents
                                    (loop :for i :upto 9
                                          :collect (erms training-x
                                                         training-y
                                                         (aref training-w i)))))
         (test-data (vgplot:load-data-file "test-data.csv"))
         (test-x (first test-data))
         (test-y (second test-data))
         (test-erms (make-array 10
                                :initial-contents
                                (loop :for i :upto 9
                                      :collect (erms test-x
                                                     test-y
                                                     (aref training-w i))))))
    (vgplot:plot m training-erms "-b;訓練;"
                 m test-erms "-r;テスト;"
                 m training-erms "ob;"
                 m test-erms "or;")
    (vgplot:legend :northwest)
    (vgplot:axis '(-1 10 0 1))
    (vgplot:grid nil)
    (vgplot:format-plot t "set xtics 3")
    (vgplot:format-plot t "set ytics 0.5")
    (vgplot:xlabel "x")
    (vgplot:ylabel "t")

    (vgplot:print-plot #P"chapter1/img/fig1-05.png" :terminal "pngcairo")
    (sleep 3)
    (vgplot:close-all-plots)))

(fig1-5)
