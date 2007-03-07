(in-package :wart)

;; login user and pass
(defparameter *user* "you")
(defparameter *pass* "your mama")

;; set the www root 
(defvar *www-root*
  (merge-pathnames
   (make-pathname :directory '(:relative "www"))
   (asdf:component-pathname (asdf:find-system :wart))))

;; set ele db requirements
(defvar *db-root*
  (merge-pathnames
   (make-pathname :directory '(:relative "db"))
   (asdf:component-pathname (asdf:find-system :wart))))
  
(defparameter *sp-db* `(:bdb ,(directory-namestring  *db-root*)))
(open-store *sp-db*)

;; the application configuration
(defparameter *wart-application*
  (make-instance 'cookie-session-application
                 :url-prefix "/wart/"
                 :www-roots (list *www-root*)
                 :dispatchers (list (action-dispatcher)
                                    ;; our app
                                    (regexp-dispatcher "^(wart.ucw|)$"
                                      (call 'page)))
                 :Debug-on-error t))
