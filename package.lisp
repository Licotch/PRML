(in-package :cl-user)

(defpackage :util
  (:use :cl)
  (:export
   :minimized-w :estimated-y))

(defpackage :prml
  (:use :cl))

(defpackage :prml.chapter1
  (:use :cl)
  (:export
   :fig1-2 :fig1-4 :fig1-5))
