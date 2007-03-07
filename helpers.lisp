(in-package :wart)

;; file handling
(defmethod save-mime ((mime-part rfc2388:mime-part) prefix)
  (let* ((header (rfc2388:get-header mime-part "Content-Disposition"))
         (filename (rfc2388:get-header-attribute header "filename"))
         (path  (merge-pathnames filename prefix)))
    (with-output-to-file  (target path  :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)(copy-stream
                                  (mime-part-body mime-part) target))
    path))

;; svg handling
(defcomponent svg-component (window-component)
  ()
  (:default-initargs :content-type "image/svg+xml"))

(defmethod render :wrapping ((svg svg-component))
  (<:ai  "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">" #\Newline)
  (call-next-method))

;; general

(defun as-keyword (sym) (intern (string sym) :keyword))

(defun formatted-time(time)
  ;;TODO : Based on localization?
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time time)
    (declare (ignore day second daylight-p zone))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D" 
	    year month date hour minute )))