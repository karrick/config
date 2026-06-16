;;; wip -- work in progress

;;; Commentary:

;;; Code:

(init-time "EXTRA FUNCTIONS"
		   (defun ksm/json-format ()
			 "Reformat contents of buffer using `fx' and `jq` commands."
			 (interactive)
			 (save-excursion
			   (shell-command-on-region (point-min)
										(point-max)
										"fx . | jq -S . | fx ."
										(buffer-name)
										t)))

		   (defun ksm/large-buffer-p ()
			 "Return non-nil when size of current buffer is large."
			 (> (buffer-size) (* 512 1024))) ; 512 KB

		   (defun ksm/remote-buffer-p ()
			 "Return non-nil when current buffer is remote (such as via TRAMP)."
			 (file-remote-p default-directory))

		   (defun ksm/strip-common-indent (beg end)
			 "Delete the common leading-whitespace rectangle from lines in BEG..END.
Finds the smallest indentation among the non-blank lines in the region
and removes that many columns from every line, the way a manual
rectangle select + `delete-rectangle' would.

Region selection:
  \\[universal-argument] prefix  -- the text inserted by the most recent yank
  active region          -- that region
  otherwise              -- the whole buffer

Strips the quote indentation prepended to pasted snippets."
			 (interactive
			  (cond
			   ;; C-u: act on the text the last yank inserted.  `yank' leaves mark
			   ;; at the start of the inserted text and point at the end.
			   ((and current-prefix-arg (mark t))
				(list (min (point) (mark t)) (max (point) (mark t))))
			   ((use-region-p)
				(list (region-beginning) (region-end)))
			   (t
				(list (point-min) (point-max)))))
			 (save-excursion
			   (let ((min-indent most-positive-fixnum)
					 (end-marker (copy-marker end)))
				 ;; Pass 1: smallest indentation of any non-blank line.
				 (goto-char beg)
				 (while (< (point) end-marker)
				   (unless (looking-at-p "[ \t]*$")          ; ignore blank lines
					 (setq min-indent (min min-indent (current-indentation))))
				   (forward-line 1))
				 ;; Pass 2: delete that column rectangle.
				 (when (and (/= min-indent most-positive-fixnum) (> min-indent 0))
				   (goto-char beg)
				   (forward-line 0)
				   (let ((corner (point)))
					 (goto-char end-marker)
					 ;; If END sits at a line start, that line isn't really in the
					 ;; region -- back up so we don't strip an extra line.
					 (when (and (bolp) (> (point) corner))
					   (forward-line -1))
					 (move-to-column min-indent)
					 (delete-rectangle corner (point))))
				 (set-marker end-marker nil))))

		   (define-key global-map (kbd "C-c d") #'ksm/strip-common-indent)

		   ;; serial-term
		   (when nil
			 ;; example use
			 (serial-process-configure :process "/dev/ttyS0" :speed 1200)))

(provide 'wip)
;;; wip.el ends here
