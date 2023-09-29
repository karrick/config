;;; hrg -- search history directory for search term

;;; Commentary:

;;; Code:

(defun hrg (search-term)
  "Search command history directory for SEARCH-TERM."
  (interactive "sSearch term: ")
  (let ((directory (file-name-concat (or
									  (getenv "XDG_STATE_HOME")
									  (expand-file-name "~/.local/state"))
									 "history")))
	(if (file-directory-p directory)
		(if (featurep 'deadgrep)
			(deadgrep search-term directory)
		  (message "Cannot use deadgrep feature"))
	  (message "Cannot find history directory: %s" directory))))

(provide 'hrg)

;;; hrg.el ends here
