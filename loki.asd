(in-package :cl-user)

(defpackage #:loki-asd
  (:use :cl :asdf)
  (:export #:+loki-version+))

(in-package :loki-asd)

(defparameter +loki-version+ "0.0.1"
  "String indicating current version of loki.")


(defsystem :loki
  :version #.+loki-version+
  :depends-on (:alexandria
               :anaphora
               :bordeaux-threads)
  :serial t
  :components
  ((:module object-system
            :serial t
            :components ((:file "boot")
                         (:file "method-creation")
                         (:file "method")
                         (:file "userland-setup")
                         (:file "print-readably")
                         (:file "reader")))
   (:file "main-package")))