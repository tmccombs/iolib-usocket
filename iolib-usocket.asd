;;; iolib-usocket.asd
;;;
;;; Copyright (c) 2015 Thayne McCombs

(defsystem iolib-usocket
  :description "usocket backend for iolib"
  :version "0.1.0"
  :author "Thayne McCombs <bytecurry.software@gmail.com>"
  :license "MIT"
  :depends-on '(:usocket :iolib)
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "iolib-usocket")))
