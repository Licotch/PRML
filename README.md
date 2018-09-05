PRML with Common Lisp
=====================

data.csv
```
(let* ((x (vgplot:range 0 10/9 1/9))
                (y (map 'vector #'(lambda (x) (+ (sin (* 2 pi x)) (alexandria:gaussian-random -0.4 0.4))) x)))
           (with-open-file (fout "data.csv" :direction :output :if-exists :supersede)
             (loop :for x :across x
                   :for y :across y
                   :do (format fout "~a,~a~%" x y))))
```

test-data.csv
```
(let* ((x (vgplot:range 0 100/99 1/99))
                (y (map 'vector #'(lambda (x) (+ (sin (* 2 pi x)) (alexandria:gaussian-random -0.4 0.4))) x)))
           (with-open-file (fout "test-data.csv" :direction :output :if-exists :supersede)
             (loop :for x :across x
                   :for y :across y
                   :do (format fout "~a,~a~%" x y))))
```
