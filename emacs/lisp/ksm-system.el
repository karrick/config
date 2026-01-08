;;; ksm-system -- introspect and return system name

;;; Commentary:

;;; Code:

(defun ksm/system-arch ()
  "Return system architecture as 'arm64' or 'amd64', or nil if unknown."
  (condition-case nil
	  (let* ((arch (downcase system-configuration)))
		(cond
		 ((string-match-p "aarch64\\|arm64" arch)
		  "arm64")
		 ((string-match-p "x86_64\\|amd64" arch)
		  "amd64")
		 (t nil)))
	(error nil)))

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

(defun ksm/system-type ()
  "Return a string representing the 'system-type' or 'unknown' if unknown."
  (cond
   ((memq system-type '(darwin ms-dos windows-nt cygwin haiku)) (symbol-name system-type))
   ((eq system-type 'gnu/kfreebsd) "freebsd")
   ((eq system-type 'gnu/linux) (or
								 ;; Some programs must be compiled for each
								 ;; major EL release version.
								 (and
								  (file-readable-p "/etc/os-release")
								  ;; awk -F= '/^(ID|VERSION_ID)=/{gsub(/"/,"",$2);a[$1]=$2}END{split(a["VERSION_ID"],v,".");printf "%s%s\n",a["ID"],v[1]}' /etc/os-release
								  (condition-case nil
									  (with-temp-buffer
										(insert-file-contents "/etc/os-release")
										(let (id version-id)
										  (dolist (line (split-string (buffer-string) "\n" t))
											(when (string-match "\\`\\(ID\\|VERSION_ID\\)=\\(.*\\)\\'" line)
											  (let ((key (match-string 1 line))
													(val (replace-regexp-in-string "\"" "" (match-string 2 line))))
												(cond
												 ((string= key "ID") (setq id val))
												 ((string= key "VERSION_ID") (setq version-id val))))))
										  (when (and id version-id)
											(let ((major (car (split-string version-id "\\."))))
											  (concat id major)))))
									(error nil)))
								 "linux"))
   (t "unknown")))

(provide 'ksm-system)
;;; ksm-system.el ends here
