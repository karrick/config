;;; env-set-when-null -- sets environment variable when null

;;; Commentary:

;;; Code:

(defun env-set-when-null (key default &optional verbose)
  "Set environment variable KEY to DEFAULT if not already set."
  (if (not verbose)
	  (or (getenv key) (setenv key default))
	(let ((value (getenv key)))
	  (if (or (null value) (string-equal "" value))
		  (message "Setting %s to %s " key (setenv key default))
		(message "Observing %s value already set to %s" key value)))))

(provide 'env-set-when-null)

;;; env-set-when-null.el ends here
