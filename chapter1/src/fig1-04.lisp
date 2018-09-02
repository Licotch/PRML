(ql:quickload '(:vgplot)
              :silent t)

(in-package :cl-user)
(defpackage :prml.chapter1
  (:use :cl)
  (:export :fig1-04))
(in-package :prml.chapter1)

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

(defun fig1-4 ()
  (let* ((x1 (vgplot:range 0 101/100 1/100))
         (y1 (map 'vector (lambda (x) (sin (* 2 pi x))) x1))
         (data (vgplot:load-data-file "data.csv"))
         (x2 (first data))
         (y2 (second data))
         (x3 x1))
    (vgplot:format-plot t "set terminal pngcairo")
    (vgplot:format-plot t "set output 'chapter1/img/fig1-04.png'")

    (vgplot:subplot 2 2 0)
    (vgplot:plot x1 y1 "b;"
                 x2 y2 "ob;"
                 x3 (estimated-y x3 (minimized-w x2 y2 0)) "r;")
    (vgplot:format-plot t "set xtics 1")
    (vgplot:format-plot t "set ytics 1")
    (vgplot:axis '(-0.1 1.1 -1.5 1.5))
    (vgplot:xlabel "x")
    (vgplot:ylabel "t")
    (vgplot:text 0.8 1 "M = 0" :tag 1)
    (vgplot:grid nil)

    (vgplot:subplot 2 2 1)
    (vgplot:plot x1 y1 "b;"
                 x2 y2 "ob;"
                 x3 (estimated-y x3 (minimized-w x2 y2 1)) "r;")
    (vgplot:text 0.8 1 "M = 1" :tag 1)
    (vgplot:grid nil)

    (vgplot:subplot 2 2 2)
    (vgplot:plot x1 y1 "b;"
                 x2 y2 "ob;"
                 x3 (estimated-y x3 (minimized-w x2 y2 3)) "r;")
    (vgplot:text 0.8 1 "M = 3" :tag 1)
    (vgplot:grid nil)

    (vgplot:subplot 2 2 3)
    (vgplot:plot x1 y1 "b;"
                 x2 y2 "ob;"
                 x3 (estimated-y x3 (minimized-w x2 y2 9)) "r;")
    (vgplot:text 0.8 1 "M = 9" :tag 1)
    (vgplot:grid nil)

    (vgplot:format-plot t "unset multiplot")
    (vgplot:format-plot t "unset output")))

(fig1-4)
