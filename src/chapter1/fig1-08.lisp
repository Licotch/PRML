(in-package :prml.chapter1)

(defun fig1-8 ()
  (let* ((ln-lambda (vgplot:range -37 -0))
         (training-set (vgplot:load-data-file (asdf:system-relative-pathname :prml "data/data.csv")))
         (x (first training-set))
         (y (second training-set))
         (test-data-set (vgplot:load-data-file (asdf:system-relative-pathname :prml "data/test-data.csv")))
         (test-x (first test-data-set))
         (test-y (second test-data-set))
         (trained-w-array (make-array (length ln-lambda)
                                      :initial-contents
                                      (loop :for ln-lambda :across ln-lambda
                                            :collect (minimized-w-tilde x y (coerce (exp ln-lambda)
                                                                                    'long-float)
                                                                        9))))
         (training-erms (loop :for w :across trained-w-array
                              :collect (erms x y w)))
         (test-erms (loop :for w :across trained-w-array
                          :collect (erms test-x test-y w))))
    (vgplot:format-plot nil "set terminal qt enhanced")

    (vgplot:plot ln-lambda training-erms "b;訓練;"
                 ln-lambda test-erms "r;テスト")
    (vgplot:axis '(-38 1 0 1))
    (vgplot:grid nil)
    (vgplot:format-plot nil "set xtics 5")
    (vgplot:format-plot nil "set ytics 0.5")
    (vgplot:xlabel "ln λ")
    (vgplot:ylabel "E{/*0.7 RMS}")))
