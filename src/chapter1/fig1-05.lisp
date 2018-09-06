(in-package :prml.chapter1)

(defun fig1-5 ()
  (let* ((m (vgplot:range 0 10))
         (training-set (vgplot:load-data-file (asdf:system-relative-pathname :prml "data/data.csv")))
         (x (first training-set))
         (y (second training-set))
         (test-data-set (vgplot:load-data-file (asdf:system-relative-pathname :prml "data/test-data.csv")))
         (test-x (first test-data-set))
         (test-y (second test-data-set))
         (trained-w-array (make-array 10
                                      :initial-contents
                                      (loop :for m :upto 9
                                            :collect (minimized-w x y m))))
         (training-erms (loop :for m :upto 9
                              :collect (erms x
                                             y
                                             (aref trained-w-array m))))
         (test-erms (loop :for m :upto 9
                          :collect (erms test-x
                                         test-y
                                         (aref trained-w-array m)))))
    (vgplot:format-plot nil "set terminal qt enhanced")

    (vgplot:plot m training-erms "-b;訓練;"
                 m test-erms "-r;テスト;"
                 m training-erms "ob;"
                 m test-erms "or;")
    (vgplot:legend :northwest)
    (vgplot:axis '(-1 10 0 1))
    (vgplot:grid nil)
    (vgplot:format-plot nil "set xtics 3")
    (vgplot:format-plot nil "set ytics 0.5")
    (vgplot:xlabel "M")
    (vgplot:ylabel "E{/*0.7 RMS}")))
