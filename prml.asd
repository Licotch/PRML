(defsystem :prml
  :depends-on ("vgplot" "alexandria")
  :components ((:file "package")
               (:module "src"
                :depends-on ("package")
                :components ((:module "chapter1"
                              :components ((:file "util")
                                           (:file "fig1-02")
                                           (:file "fig1-04" :depends-on ("util"))
                                           (:file "fig1-05" :depends-on ("util"))
                                           (:file "fig1-06" :depends-on ("util"))
                                           (:file "fig1-07" :depends-on ("util"))))))))
