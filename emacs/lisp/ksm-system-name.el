;;; ksm-system-name -- introspect and return system name

;;; Commentary:

;;; Code:

(defun ksm/system-name ()
  "Return a string representing the 'system-type'."
  (cond
   ((memq system-type '(darwin ms-dos windows-nt cygwin haiku)) (symbol-name system-type))
   ((eq system-type 'gnu/kfreebsd) "freebsd")
   ((eq system-type 'gnu/linux) (or
								 ;; Some programs must be compiled for each
								 ;; major EL release version.
								 (and
								  (file-readable-p "/etc/os-release")
								  (with-temp-buffer
									(insert-file-contents "/etc/os-release")
									(goto-char (point-min))
									(when
										(re-search-forward "^VERSION_ID[ \t]\\([0-9]+\\)" nil 'NOERROR)
									  (concat "el" (match-string 1)))))
								 (when (file-readable-p "/etc/debian-release")
								   "debian")
								 "linux"))
   (t "unknown")))

(provide 'ksm-system-name)

;;; ksm-system-name.el ends here
