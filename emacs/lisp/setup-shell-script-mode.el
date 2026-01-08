;;; setup-shell-script-mode -- customizations for shell programming

;;; Commentary:

;;; Code:

;; TODO: Consider https://github.com/cuonglm/flycheck-checkbashisms

(let ((cmd (executable-find "shellcheck")))
  (if (null cmd)
	  (message "Cannot find 'shellcheck' program.")
	(setq flycheck-sh-shellcheck-executable cmd)
	(add-hook 'sh-mode-hook 'flycheck-mode)))

;; go install mvdan.cc/sh/v3/cmd/shfmt@latest
(when (and nil (executable-find "shfmt"))
  (use-package shfmt
	:ensure t
	:hook (sh-mode shfmt-on-save-mode)))

(provide 'setup-shell-script-mode)
;;; setup-shell-script-mode.el ends here
