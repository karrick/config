;;; setup-flycheck -- setup flycheck package

;;; Commentary:

;;; Code:

(use-package flycheck
  :ensure t

  :config

  (global-flycheck-mode)
  (let ((cmd (executable-find "shellcheck")))
	(if (null cmd)
		(message "Cannot find 'shellcheck' program.")
	  (setq flycheck-sh-shellcheck-executable cmd)))

  :hook (sh-mode . flycheck-mode))

(provide 'setup-flycheck)

;;; setup-flycheck.el ends here
