(in-package :prml.chapter1)

(defun row-dimention (matrix)
  (array-dimension matrix 1))

(defun last-value-of-row (matrix nth-row)
  (aref matrix nth-row (1- (row-dimention matrix))))

(defun row (matrix nth-row)
  (let ((row-dimention (row-dimention matrix)))
    (make-array row-dimention
               :initial-contents
               (loop :for current-column :from 0 :below row-dimention
                     :collect (aref matrix nth-row current-column)))))

(defun swap-rows! (matrix swap-from swap-to)
  (let ((row-swap-to (row matrix swap-to)))
    (dotimes (current-column (row-dimention matrix) matrix)
      (setf (aref matrix swap-to current-column) (aref matrix swap-from current-column))
      (setf (aref matrix swap-from current-column) (aref row-swap-to current-column)))))

(defun add-row-to-multiplied-row! (matrix nth-row-to-be-added nth-row-to-be-multiplied n)
  (dotimes (current-column (row-dimention matrix) matrix)
    (setf (aref matrix nth-row-to-be-added current-column)
          (+ (aref matrix nth-row-to-be-added current-column)
             (* (aref matrix nth-row-to-be-multiplied current-column) n)))))

(defun multiply-to-row! (matrix nth-row n)
  (dotimes (current-column (row-dimention matrix) matrix)
    (setf (aref matrix nth-row current-column) (* (aref matrix nth-row current-column) n))))

(defun gaussian-elimination! (matrix)
  (let* ((m (array-dimension matrix 0))
         (w (make-array m))
         (current-column))
    (block gaussian-elimination
      ;; gaussian elimination
      (dotimes (current-row m)
        (setf current-column current-row)

        ;; if 0 in current row and current column then swap to another row
        (when (zerop (aref matrix current-row current-column))
          (let ((row-swap-to (1+ current-row)))
            (tagbody swap-part
               (if (< row-swap-to m)
                   (when (zerop (aref matrix row-swap-to current-column))
                     (incf row-swap-to)
                     (go swap-part))
                   (return-from gaussian-elimination nil))
               (swap-rows! matrix current-row row-swap-to))))

        ;; get a 1 in current row, current column.
        (and (not (= 1 (aref matrix current-row current-column)))
             (multiply-to-row! matrix current-row (/ 1 (aref matrix current-row current-column))))

        ;; get a 0 in another row, current column.
        (dotimes (row-added-to-current-row m)
          (and (not (= row-added-to-current-row current-row))
               (not (zerop (aref matrix row-added-to-current-row current-column)))
               (add-row-to-multiplied-row! matrix
                                           row-added-to-current-row
                                           current-row
                                           (* -1 (aref matrix row-added-to-current-row current-column))))))
      ;;; make the retern data
      (dotimes (i m w)
        (setf (aref w i) (last-value-of-row matrix i))))))

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

(defun estimated-yn (xn w)
  (let ((yn (aref w 0)))
    (do ((i 1 (1+ i)))
        ((>= i (length w)) yn)
      (setf yn (+ yn (* (aref w i) (expt xn i)))))))

(defun estimated-y (x w)
  (map 'vector (lambda (xn) (estimated-yn xn w)) x))

(defun fig1-4 ()
  (let* ((sin-x (vgplot:range 0 101/100 1/100))
         (sin-y (map 'vector (lambda (x) (sin (* 2 pi x))) sin-x))
         (training-set (vgplot:load-data-file (asdf:system-relative-pathname :prml "data/data.csv")))
         (observed-x (first training-set))
         (observed-y (second training-set))
         (estimated-x sin-x))
    (labels ((subplot (subplot-n m)
               (vgplot:subplot 2 2 subplot-n)
               (vgplot:plot sin-x sin-y "g;"
                            observed-x observed-y "ob;"
                            estimated-x (estimated-y estimated-x (minimized-w observed-x observed-y m)) "r;")
               (vgplot:text 0.8 1 (format nil "M = ~a" m) :tag 1)
               (vgplot:grid nil)))
      (vgplot:subplot 2 2 0)
      (vgplot:format-plot nil "set xtics 1")
      (vgplot:format-plot nil "set ytics 1")
      (vgplot:axis '(-0.1 1.1 -1.5 1.5))
      (vgplot:xlabel "x")
      (vgplot:ylabel "t")

      (subplot 0 0)
      (subplot 1 1)
      (subplot 2 3)
      (subplot 3 9))))
