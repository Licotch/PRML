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
      (gaussian-elimination! matrix))))

(defun estimated-yn (xn w)
  (let ((yn (aref w 0)))
    (do ((i 1 (1+ i)))
        ((>= i (length w)) yn)
      (setf yn (+ yn (* (aref w i) (expt xn i)))))))

(defun estimated-y (x w)
  (map 'vector (lambda (xn) (estimated-yn xn w)) x))

(defun e (x y w)
  (* 1/2 (loop :for n :below (length x)
               :summing (expt (- (estimated-yn (aref x n) w) (aref y n)) 2))))

(defun erms (x y w)
  (sqrt (/ (* 2 (e x y w)) (length x))))

(defun minimized-w-tilde (x y lambda m)
  (labels ((aij (i j)
             (reduce #'+ (map 'vector #'(lambda (xn) (expt xn (+ i j))) x)))
           (aij-tilde (i j lambda)
             (+ (aij i j) (if (= i j) lambda 0)))
           (ti (i)
             (reduce #'+ (map 'vector #'(lambda (xn yn) (* (expt xn i) yn)) x y))))
    (let ((matrix (make-array (list (+ 1 m) (+ 2 m)))))
      (dotimes (i (+ 1 m))
        (dotimes (j (+ 2 m))
          (setf (aref matrix i j) (aij-tilde i j lambda)))
        (setf (aref matrix i (+ 1 m)) (ti i)))
      (gaussian-elimination! matrix))))

(defun gaussian-distribution (mu sigma x)
  (* (/ 1 (sqrt (* 2 pi (expt sigma 2)))) (exp (* -1 (/ (expt (- x mu) 2) (* 2 (expt sigma 2)))))))
