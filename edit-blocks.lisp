(in-package :wart)

(defcomponent editable-text (logged-in-propagate-mixin)
  ((text-class :accessor text-class-of
               :initarg :text-class)
   (c-page :accessor c-page-of :initform 0)
   (text-per-page :accessor text-per-page-of
                  :initarg :text-per-page
                  :initform 10)
   (paging-p :accessor paging-p-of
             :initarg :paging-p
             :initform t))
  (:documentation "base class for in place editable text blocks. 
                   needs user to be logged in, through login machinery"))

(defmethod render ((e editable-text))
  (dolist (text-list (nreverse (get-instances-by-class (text-class-of e))))
    (render-item e text-list :in-list t))
  (when (logged-in e)
    (progn
      (<ucw:a :action (add-text e) "add")
      (<:br)
      (<:br)
      (<:ah "for the technical amongst you, the item(s) above use the markup language 'markdown'")
      (<:br)
      (<:ah "see ")
      (<ucw:a :href "http://daringfireball.net/projects/markdown/" "the markdown project page"))))

(defun render-item (e text-list &key in-list curr-comp)
  (when (or (visible-p text-list) (logged-in e)) 
    (<:div :class "body-text-block"
           (<:div :class "body-text-block-inner"
                  (when (and (logged-in e) (not (visible-p text-list)))
                    (<:ah "[deleted]"))
                  (dolist (text (class-slot-names (text-class-of e)))
                    (unless (or (eql text 'visible-p) (eql text 'link-through))
                      (view-text (slot-value text-list text) :in-list in-list))
                    (if (eql text 'link-through)
                        (generate-link-through (slot-value text-list text) e text-list in-list curr-comp)))
                  (when (logged-in e)
                    (<ucw:a :action (edit-text e text-list) "edit")
                    (<:ah " ")
                    (if (visible-p text-list)
                        (<ucw:a :action (del-text e text-list) "delete")
                        (<ucw:a :action (revamp-text e text-list) "restore"))
                    (<:br))))))

;;;; edit-mode stuff
(defclass text-unit ()
  ((visible-p :accessor visible-p :initarg :visible-p :initform t))
  (:metaclass persistent-metaclass))

(defaction add-text ((e editable-text))
  (call 'edit-form :contents (make-instance (text-class-of e))))

(defaction del-text ((e editable-text) text)
  (setf (visible-p text) nil))

(defaction revamp-text ((e editable-text) text)
  (setf (visible-p text) t))

(defaction edit-text ((e editable-text) text)
  (call 'edit-form :contents text))

(defcomponent edit-form ()
  ((contents :accessor contents-of :initarg :contents)
   (process-flag :accessor process-flag :accessor process-flag :initform :edit)
   (temp :accessor temp-of)))

(defmethod render ((e edit-form))
  (case (process-flag e)
    (:edit (<ucw:form :action (ok e)
                      :method "post"
                      (dolist (text (instance-slot-names (contents-of e)))
                        (edit-field (slot-value (contents-of e) text))
                        (<:br))
                      (<:br)
                      (<:submit :value "submit")))
    (:check (progn
              (view-text (temp-of e))))))

;;;; view individual items of an item list
(defcomponent view-item ()
  ((editable :accessor editable-of :initarg :editable)
   (item :accessor item-of :initarg :item))
  (:render ()
           (render-item (editable-of view-item) (item-of view-item) :curr-comp view-item)))

(defaction show-item ((e editable-text) i)
  (call 'view-item :editable e :item i))

;; generic text-field
(defclass text-field ()
  ((contents :accessor contents-of :initarg :contents :initform "")
   (markfile :accessor markfile-of :initarg :markfile :initform "")
   (non-list-p :accessor non-list-p :initarg :non-list-p :initform nil))
  (:metaclass persistent-metaclass)
  (:documentation "base class of input text object"))

(defmethod view-text ((text text-field) &key in-list)
  (<:ai (markfile-of text)))

(defmethod view-text :around ((text text-field) &key in-list)
  (if in-list
      (when (not (non-list-p text))
        (call-next-method))
      (call-next-method text)))

;; text-area
(defun construct-body (&key non-list-p)
  (make-instance 'text-area :non-list-p non-list-p))

(defclass text-area (text-field) ()
  (:metaclass persistent-metaclass))

(defmethod edit-field ((text text-area))
  (progn
    (<ucw:textarea :reader (markfile-of text)
                   :writer #'(lambda (x)                             
                               (setf (markfile-of text) x)
                               (setf (contents-of text) (mark-it-down x text)))
                   :rows "20"
                   :cols "80")
    (<:ah "<-- big text-blob")))

(defmethod view-text ((text text-area) &key in-list)
  (<:div :class "body-text-body" (<:ai (contents-of text))))

;; text header
(defun construct-title (&key non-list-p)
  (make-instance 'text-header :non-list-p non-list-p))

(defclass text-header (text-field) ()
  (:metaclass persistent-metaclass))

(defmethod edit-field ((text text-header))
  (progn
    (<ucw:text :reader (markfile-of text)
               :writer #'(lambda (x)                             
                           (setf (markfile-of text) x)
                           (setf (contents-of text) (mark-it-down x text)))
               :size "60")
    (<:ah "<-- title")))

(defmethod view-text ((text text-header) &key in-list)
  (<:div :class "body-text-header" (<:ai (contents-of text))))

;; text intro
(defun construct-intro (&key non-list-p)
  (make-instance 'text-intro :non-list-p non-list-p))

(defclass text-intro (text-field) ()
  (:metaclass persistent-metaclass))

(defmethod edit-field ((text text-intro))
  (progn
    (<ucw:textarea :reader (markfile-of text)
                   :writer #'(lambda (x)                             
                               (setf (markfile-of text) x)
                               (setf (contents-of text) (mark-it-down x text)))
                   :rows "8"
                   :cols "80")
    (<:ah "<-- intro")))

(defmethod view-text ((text text-intro) &key in-list)
  (<:div :class "body-text-intro" (<:ai (contents-of text))))

;; text date
(defun construct-date (&key non-list-p)
  (make-instance 'text-date :non-list-p non-list-p :markfile (get-universal-time) :contents (get-universal-time)))

(defclass text-date (text-field) ()
  (:metaclass persistent-metaclass))

(defmethod edit-field ((text text-date))
  (<:ah "date is fixed, complain and i'll fix it"))

(defmethod view-text ((text text-date) &key in-list)
  (<:div :class "body-text-date" (<:ai (formatted-time (contents-of text)))))

;; text origin
(defun construct-origin (&key non-list-p)
  (make-instance 'text-origin :non-list-p non-list-p))

(defclass text-origin (text-field) ()
  (:metaclass persistent-metaclass))

(defmethod edit-field ((text text-origin))
  (progn
    (<ucw:text
   
     :reader (markfile-of text)
     :writer #'(lambda (x)                             
                 (setf (markfile-of text) x)
                 (setf (contents-of text) (mark-it-down x text)))
     :size "20")
    (<:ah "<-- initial medium of article")))

(defmethod view-text ((text text-origin) &key in-list)
  (<:div :class "body-text-origin" (<:ai (contents-of text))))

;; link-through
(defun construct-link-through (&key non-list-p)
  (make-instance 'text-link-through :non-list-p non-list-p :contents "whole text"))

(defclass text-link-through (text-field) ()
  (:metaclass persistent-metaclass))

(defmethod edit-field ((text text-link-through))
  (progn
    (<ucw:text
   
     :reader (contents-of text)
     :writer #'(lambda (x)                             
                 (setf (contents-of text) x))
     :size "20")
    (<:ah "<-- link name")))

(defun generate-link-through (link-through e text in-list &optional curr-comp)
  (<:div :class "body-link-through" (if in-list
                                        (<ucw:a :action (show-item e text) (<:ai (contents-of link-through)))
                                        (<ucw:a :action (ok curr-comp) (<:ai "<-- back")))))

;; quicktype macros
(defun concat-symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))

(defun text-slot (slot-spec)
  (let ((slot-name (if (symbolp slot-spec) slot-spec (first slot-spec)))
        (slot-class (if (symbolp slot-spec) slot-spec (second slot-spec)))
        (rest (if (listp slot-spec) (cddr slot-spec))))
    `(,slot-name :accessor ,(concat-symbol slot-name '-of) :initform (,(concat-symbol 'construct- slot-class) ,@rest) :index t)))

(defmacro define-text-unit (name slots)
  `(defclass ,name (text-unit)
     ,(mapcar #'text-slot slots)
     (:metaclass persistent-metaclass)))

(defmacro define-editable-text (et-class text-class)
  `(defcomponent ,et-class (editable-text) ()
                 (:default-initargs :text-class ',text-class)))

(defmacro define-text-block-solid (et-class text-class slots)
  `(progn
     (define-editable-text ,et-class ,text-class)
     (define-text-unit ,text-class ,slots)))

(defmacro def-text-component (name title text-slots)
  (let ((editable-name (concat-symbol 'editable- name)))
    `(progn
       (defcomponent ,name (logged-in-propagate-mixin)
         ((title :accessor title :initform ,title)
          (text :accessor text :component ,editable-name))
         (:render ()
                  (render (text ,name))))
       (define-text-block-solid ,editable-name ,(concat-symbol name '-text) ,text-slots))))

;; got from symbolics code somewhere. How cool is that!! (hope they don't sue)
(defmethod instance-slot-names ((instance standard-object))
  "Given an INSTANCE, returns a list of the slots in the instance's class."
  (mapcar #'mopp:slot-definition-name
          (mopp:class-direct-slots (class-of instance))))

(defun class-slot-names (class-name)
  "Given a CLASS-NAME, returns a list of the slots in the class."
  (mapcar #'mopp:slot-definition-name
          (mopp:class-direct-slots (find-class class-name))))

;; some more helper functions
(defun mark-it-down (x text)
  (multiple-value-bind (a b)
      (cl-markdown:markdown x :stream nil)
    (declare (ignore a))
    (format *standard-output* "this is the value of ~A" b)
    (setf (contents-of text) b)))

;;obsolete, due to cl-markdown bugfix
(defun return-to-newline (x)
  (ppcre:regex-replace-all (format nil "~a" #\return) x (format nil "~a" #\newline)))