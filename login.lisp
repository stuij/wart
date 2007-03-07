(in-package :wart)
 
;;;; an updated login
(defcomponent cerberos ()
  ((logged-in :initform nil :accessor logged-in :initarg :logged-in)
   (message :initform nil :accessor login.message :initarg :message)
   (username :accessor login.username
             :initform (make-instance 'string-field
                                      :css-class "ucw-login-username"
                                      :input-size 10))
   (password :accessor login.password
             :initform (make-instance 'password-field
                                      :css-class "ucw-login-password"
                                      :input-size 10))))

(defaction login-successful ((c cerberos))
  (setf (logged-in c) t))

(defaction logout ((c cerberos))
  (setf (logged-in c) nil)
  (flush-session))

(defmethod check-credentials ((login cerberos))
  (and (string= (value (login.username login)) *user*)
       (string= (value (login.password login)) *pass*)))

(defaction try-login ((l cerberos))
  (if (check-credentials l)
      (login-successful l)
      (setf (login.message l) "Login failed!!")))

(defmethod render ((l cerberos))
  (if (logged-in l)
      (<ucw:simple-form (<ucw:simple-submit :action (logout l) "log me out"))
      (<:div :id "ucw-login"
             (when (login.message l)
               (<:div :id "ucw-login-message" (<:as-html (login.message l))))
             (<ucw:form :action (try-login l)
                        :method "POST"
                        (render (login.username l))
                        (render (login.password l))
                        (<:input :type "submit" :value "login")))))

(defcomponent hell-gate ()
  ((login-aside :accessor log-me-into :component cerberos)
   (logged-in :initform nil :accessor logged-in :initarg :logged-in))
  (:documentation "base class for components that want to nest secure-component-mixin components"))

(defmethod render :around ((s hell-gate))
  (if (logged-in (log-me-into s))
      (setf (logged-in s) t)
      (setf (logged-in s) nil))
  (call-next-method))

;;;; trying some render smartness
;;; first off the base logged-in component
(defcomponent logged-in-propagate-mixin ()
  ((logged-in :initform nil :accessor logged-in :initarg :logged-in)))

(defmethod render :before ((s logged-in-propagate-mixin))
  (if (logged-in (parent s))
      (setf (logged-in s) t)
      (setf (logged-in s) nil)))


;;; mixin for components only to be seen when logged in
(defcomponent secure-component-mixin (logged-in-propagate-mixin)
  ())

(defmethod render :around ((s secure-component-mixin))
  (if (logged-in (parent s))
      (progn
        (setf (logged-in s) t)
        (call-next-method))
      (setf (logged-in s) nil)))

;;; secure flipflop component, shows a different component depending on if we're logged in
(defcomponent secure-flipflop-mixin (logged-in-propagate-mixin)
  ((secure-component :initarg :secure-component :accessor secure-component-of)
   (insecure-component :initarg :insecure-component :accessor insecure-component-of)))

(defmethod render ((s secure-flipflop-mixin))
  (if (logged-in s)
      (render (secure-component-of s))
      (render (insecure-component-of s))))

;;;; the to render base component
(defcomponent hell (simple-window-component hell-gate)
  ((piggy-bagger-flip :accessor piggy-bagger-flip :component flip-hell)
   (piggy-bagger :accessor piggy-bagger :component print-me))
  (:default-initargs
      :title "come closer"
    :stylesheet "sheet.css")
  (:render ()
           (render (log-me-into hell))
           (<:p
            (render (piggy-bagger hell)))
           (<:p
            (render (piggy-bagger-flip hell)))))

;;;; the component sometimes not to be rendered inside the to render component, thanx to smartness
;;; the secure-mixin-component
(defcomponent print-me (secure-component-mixin)
  ()
  (:render ()
           (<:ah "I am the god of hellfire!! And I give you...")))

;;; the secure flipflop component
(defcomponent print-me-secure ()
  ()
  (:render ()
           (<:ah "a teenage prom queen")))

(defcomponent print-me-insecure ()
  ()
  (:render ()
           (<:ah "a pimply faced dweeb")))

(defcomponent flip-hell (secure-flipflop-mixin) ()
              (:default-initargs
                  :secure-component (make-instance 'print-me-secure) 
                :insecure-component (make-instance 'print-me-insecure)))