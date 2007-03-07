(in-package :wart)

(defcomponent page (simple-window-component hell-gate)
  ((section :accessor section-of :initarg :section :component editable-text)
   (menu-pane :accessor menu-pane-of
              :component (base-menu :current-component-key "home"
                                    :title-base ":: wart - "
                                    :contents `(("home"        . ,(make-instance 'home))
                                                ("documents" . ,(make-instance 'documents))
                                                ("contact"    . ,(make-instance 'contact))))))
  (:default-initargs :title ":: Frige de Fem ::" :stylesheet '("sheet.css" "wart-dress.css")
                     :javascript '((:script "function showBranch(branch)
              			                  {
		        	                   var objBranch = document.getElementById(branch).style;
			                           if(objBranch.display==\"block\")
				                     objBranch.display=\"none\";
			                           else
				                     objBranch.display=\"block\";
			                          }")))
  (:render ()
           (<:div :class "main-body" (<:p (render (menu-pane-of page)))
                  (<:p (<:span :onclick "showBranch('login')" "login")
                       (<:span :class "branch" :style "display: none;" :id "login"
                               (render (log-me-into page)))))))

(defcomponent menu (tabbed-pane logged-in-propagate-mixin)
  ((title-base :accessor title-base-of :initarg :title-base))
              (:render () (call-next-method)))

(defcomponent base-menu (menu) ())

(defaction switch-component :after ((container base-menu) key)
           (setf (window-component.title (parent container)) (concatenate 'string (title-base-of container) key " ::")))

;; --- main pages
;; home
(defcomponent home (logged-in-propagate-mixin)
  ((title :accessor title :initform "home")
   (text :accessor text :component editable-home)
   (nyheter :accessor nyheter :component home-news))
  (:render ()
           (<:div
            (<:table
             (<:tr
              (<:td :class "nyheter" (render (nyheter home)))
              (<:td :class "intro-text" (render (text home))))))))

(define-text-block-solid editable-home home-text      (title sidebar))
(define-text-block-solid home-news     home-news-text (title date intro (body body :non-list-p t) link-through))

;; custom text element for hem sidebar main text 
(defun construct-sidebar (&key non-list-p)
  (make-instance 'text-sidebar :non-list-p non-list-p))

(defclass text-sidebar (text-field) ()
  (:metaclass persistent-metaclass))

(defmethod edit-field ((text text-sidebar))
  (progn
    (<ucw:textarea :reader (markfile-of text)
                   :writer #'(lambda (x)                             
                               (setf (markfile-of text) x)
                               (setf (contents-of text) (mark-it-down x text)))
                   :rows "40"
                   :cols "40")
    (<:ah "<-- big text-blob")))

(defmethod view-text ((text text-sidebar) &key in-list)
  (<:div :class "body-text-sidebar" (<:ai (contents-of text))))

;; comprised ones
(def-text-component documents "documents" (title date intro (document body :non-list-p t) link-through))
(def-text-component contact   "contact"   (body))