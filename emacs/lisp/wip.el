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

		   ;; serial-term
		   (when nil
			 ;; example use
			 (serial-process-configure :process "/dev/ttyS0" :speed 1200)))

(provide 'wip)
;;; wip.el ends here
