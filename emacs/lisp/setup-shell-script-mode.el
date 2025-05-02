;;; setup-shell-script-mode -- customizations for shell programming

;;; Commentary:

;;; Code:

;; go install mvdan.cc/sh/v3/cmd/shfmt@latest
(when (and nil (executable-find "shfmt"))
  (use-package shfmt
	:ensure t
	:hook (sh-mode shfmt-on-save-mode)))

(provide 'setup-shell-script-mode)

;;; setup-shell-script-mode.el ends here
