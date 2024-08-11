;;; paths -- set exec-path and PATH environment variable for child processes

;;; Commentary:
;;;
;;; After XDG_DATA_HOME is set, can set PATH environment variable to any of
;;; the directories I typically use, provided that they exist.

;;; Code:

(require 'empty-string)
(require 'env-set-when-null)
(require 'ksm-system-name)
(require 'path)

(let ((xdg-data-home (getenv "XDG_DATA_HOME")))
  (if (empty-string-p xdg-data-home)
	  (message "WARNING: XDG_DATA_HOME is empty.")
	(let* ((base (list
				  (file-name-concat "~" ".cargo" "bin")
				  (file-name-concat xdg-data-home (ksm/system-name) "bin")
				  (file-name-concat xdg-data-home "bin")
				  (file-name-concat "~" "bin")
				  ))
		   (paths (if (eq system-type 'darwin)
					  (append
					   (list
						"/opt/local/sbin"
						"/opt/local/bin"
						"/opt/local/libexec/gnubin"
						)
					   base)
					base)))
	  (dolist (path (mapcar #'(lambda (path) (expand-file-name path)) paths))
		(when (file-accessible-directory-p path)
		  (path-prepend path))))))

(provide 'paths)

;;; paths.el ends here
