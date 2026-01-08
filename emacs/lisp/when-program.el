;;; when-program -- perform body when program found.

;;; Commentary:

;;; Code:

;; (defmacro when-program (program &rest body)
;;   "When PROGRAM is found in path, execute BODY with its location."
;;   (declare (indent 1))
;;   `(let ((cmd (executable-find ,program)))
;;	 (if (null cmd)
;;		 (message "Cannot find '%s' program." ,program)
;;	   ,@body cmd)))

(defmacro when-program (program &rest callback)
  "When PROGRAM is found in path, execute BODY with its location."
  (declare (indent 1))
  `(let ((cmd (executable-find ,program)))
	 (if (null cmd)
		 (message "Cannot find '%s' program." ,program)
	   (funcall ,callback cmd))))

(provide 'when-program)

;;; when-program.el ends here
