;;; -*- lisp -*-

;;;; * Default UCW startup file

(in-package :common-lisp-user)

;;;; Load up UCW itself plus the examples
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :ucw)
  (asdf:oos 'asdf:load-op :wart))

;;;; Let there be swank
(ucw:start-swank)

;;;; Finally startup the server
(ucw:create-server)

;;;; And register your application
(ucw:register-application ucw:*default-server* wart::*wart-application*)