;;; sort-commas -- sort a list of delimited strings in region

;;; Commentary:

;;; Code:

(defun sort-commas (start end)
  "Sort a list of comma delimited strings between START and END."
  (interactive "*r")
  (if (use-region-p)
	  (let ((selection (buffer-substring-no-properties start end)))
		(delete-region start end)
		(insert (string-join (sort (mapcar #'string-trim (split-string selection ",")) #'string<) ",")))
	(message "cannot sort-commas without region selected")))

(defun sort-spaces (start end)
  "Sort a list of space delimited strings between START and END."
  (interactive "*r")
  (if (use-region-p)
	  (let ((selection (buffer-substring-no-properties start end)))
		(delete-region start end)
		(insert (string-join (sort (mapcar #'string-trim (split-string selection " ")) #'string<) " ")))
	(message "cannot sort-spaces without region selected")))

(provide 'sort-commas)

;;; sort-commas.el ends here
