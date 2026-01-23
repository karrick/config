;;; local -- configuration for the local environment

;;; Commentary:

;;; Code:

(init-time "LOCAL"
		   ;; On Windows prefer using `plink.exe` program for TRAMP
		   ;; connections.
		   (when (and (eq system-type 'windows-nt) (executable-find "plink"))
			 (setq tramp-default-method "plink")))

(provide 'local)
;;; local.el ends here
