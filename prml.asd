(defsystem :prml
  :depends-on (#:vgplot #:alexandria)
  :serial t
  :components ((:file "package")
               (:module "src"
                :serial t
                :components ((:module "chapter1"
                              :serial t
                              :components ((:file "fig1-02")
                                           (:file "fig1-04")
                                           (:file "fig1-05")
                                           (:file "fig1-06")))))))
