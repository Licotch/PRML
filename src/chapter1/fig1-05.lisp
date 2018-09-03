(in-package :prml.chapter1)

(ql:quickload '(:vgplot)
              :silent t)

(defun row-last (matrix row)
  (aref matrix row (1- (array-dimension matrix 1))))

(defun row (matrix row)
  (let ((row-dimention (array-dimension matrix 1)))
    (make-array row-dimention
               :initial-contents
               (loop :for column :from 0 :below row-dimention
                     :collect (aref matrix row column)))))

(defun swap-rows! (matrix row1 row2)
  (let ((tmp-row (row matrix row1)))
    (dotimes (column (array-dimension matrix 1) matrix)
      (setf (aref matrix row1 column) (aref matrix row2 column))
      (setf (aref matrix row2 column) (aref tmp-row column)))))

(defun add-row-to-multiplied-row! (matrix row1 row2 n)
  (let ((multiplied-row (make-array (array-dimension matrix 1)
                                    :initial-contents
                                    (loop :for element :across (row matrix row2)
                                          :collect (* element n)))))
    (dotimes (column (array-dimension matrix 1) matrix)
      (setf (aref matrix row1 column) (+ (aref matrix row1 column) (aref multiplied-row column))))))

(defun multiply-to-row! (matrix row n)
  (dotimes (column (array-dimension matrix 1) matrix)
    (setf (aref matrix row column) (* (aref matrix row column) n))))

(defun gaussian-elimination (matrix)
  (let* ((m (array-dimension matrix 0))
         (w (make-array m))
         (current-column))
    (block gaussian-elimination
      ;;; gaussian-elimination
      (dotimes (current-row m)
        (setf current-column current-row)

        (when (zerop (aref matrix current-row current-column))
          (let ((row-swap-to (1+ current-row)))
            (tagbody swap-part
               (if (< row-swap-to m)
                   (when (zerop (aref matrix row-swap-to current-column))
                     (incf row-swap-to)
                     (go swap-part))
                   (return-from gaussian-elimination nil))
               (swap-rows! matrix current-row row-swap-to))))

        (and (not (= 1 (aref matrix current-row current-column)))
             (multiply-to-row! matrix current-row (/ 1 (aref matrix current-row current-column))))

        (dotimes (row-added-to-current-row m)
          (and (not (= row-added-to-current-row current-row))
               (not (zerop (aref matrix row-added-to-current-row current-column)))
               (add-row-to-multiplied-row! matrix
                                           row-added-to-current-row
                                           current-row
                                           (* -1 (aref matrix row-added-to-current-row current-column))))))
      ;;; make retern data
      (dotimes (i m w)
        (setf (aref w i) (row-last matrix i))))))

(defun minimized-w (x y m)
  (labels ((aij (i j)
             (reduce #'+ (map 'vector #'(lambda (xn) (expt xn (+ i j))) x)))
           (ti (i)
             (reduce #'+ (map 'vector #'(lambda (xn yn) (* (expt xn i) yn)) x y))))
    (let ((matrix (make-array (list (+ 1 m) (+ 2 m)))))
      (dotimes (i (+ 1 m))
        (dotimes (j (+ 2 m))
          (setf (aref matrix i j) (aij i j)))
        (setf (aref matrix i (+ 1 m)) (ti i)))
      (gaussian-elimination matrix))))

(defun estimated-y (x w)
  (map 'vector
       (lambda (xi)
         (let ((y (aref w 0)))
           (dotimes (i (1- (length w)) y)
             (setf y (+ y (* (aref w (1+ i)) (expt xi (1+ i))))))))
       x))

(defun e (x y w)
  (* 1/2 (loop :for n :below (length x)
               :summing (expt (- (aref (estimated-y x w) n) (aref y n)) 2))))

(defun erms (x y w)
  (sqrt (/ (* 2 (e x y w)) (length x))))

(defun fig1-5 ()
  (let* ((m (vgplot:range 0 10))
         (training-data (vgplot:load-data-file (asdf:system-relative-pathname :prml "data/data.csv")))
         (training-x (first training-data))
         (training-y (second training-data))
         (training-w (make-array 10
                                 :initial-contents
                                 (loop :for m :across m
                                       :collect (minimized-w training-x training-y m))))
         (training-erms (make-array 10
                                    :initial-contents
                                    (loop :for i :upto 9
                                          :collect (erms training-x
                                                         training-y
                                                         (aref training-w i)))))
         (test-data (vgplot:load-data-file (asdf:system-relative-pathname :prml "data/test-data.csv")))
         (test-x (first test-data))
         (test-y (second test-data))
         (test-erms (make-array 10
                                :initial-contents
                                (loop :for i :upto 9
                                      :collect (erms test-x
                                                     test-y
                                                     (aref training-w i))))))
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
