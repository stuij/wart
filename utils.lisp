
(defmacro remove-method-easily (method class)
  `(remove-method (function ,method) (find-method (function ,method) () (list (class-of ,class)))))

(defun rm-method-qualifier (da-method class-name qualifier)
  (remove-method da-method (find-method da-method (list qualifier) (list (class-of class-name)))))
