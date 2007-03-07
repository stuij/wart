(defpackage :wart.system
  (:use :cl :asdf))

(in-package :wart.system)

(defsystem wart
  :version "0.0.0.0.3"
  :depends-on (:cl-markdown :elephant :ucw :rfc2388)
  :author "Ties Stuij"
  :components ((:file "packages")
               (:file "config"       :depends-on ("packages"))
               (:file "helpers"      :depends-on ("config"))
               (:file "login"        :depends-on ("helpers"))
               (:file "edit-blocks"  :depends-on ("login"))
               (:file "page"         :depends-on ("edit-blocks"))))