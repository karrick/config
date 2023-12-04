;;; setup-golang-mode -- customizations for Go programming language

;;; Commentary:

;;; Code:

(use-package go-mode
  :after eglot

  :config
  (progn
	(cond
	 ((and nil (featurep 'lsp-mode))
	  (lsp-register-custom-settings
	   '(("gopls.completeUnimported" t t)
		 ("gopls.staticcheck" t t)
		 ("gopls.usePlaceholders" t t)))
	  ;; Set up before-save hooks to format buffer and add modify
	  ;; imports. Ensure there is not another gofmt(1) or goimports(1) hook
	  ;; enabled.
	  (defun lsp-go-install-save-hooks ()
		(add-hook 'before-save-hook #'lsp-format-buffer t t)
		(add-hook 'before-save-hook #'lsp-organize-imports t t))
	  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
	  (add-hook 'go-mode-hook #'lsp))
	 (t
	  ;; Use gogetdoc as it provides better documentation than godoc and
	  ;; godef.
	  (when (executable-find "gogetdoc")
		(setq godoc-at-point-function #'godoc-gogetdoc))

	  ;; Prefer goimports, but when not found, use gofmt.
	  (setq gofmt-command (or (executable-find "goimports")
							  (executable-find "gofmt")))
	  (add-hook 'go-mode-hook
				#'(lambda ()
					(if (functionp 'eglot-format-buffer)
						(add-hook 'before-save-hook #'eglot-format-buffer nil t)
					  (add-hook 'before-save-hook #'gofmt-before-save nil t))))))

	(when nil
	  ;; Fix parsing of error and warning lines in compiler output.
	  (setq compilation-error-regexp-alist-alist ; first remove the standard conf; it's not good.
			(remove 'go-panic
					(remove 'go-test compilation-error-regexp-alist-alist)))
	  ;; Make another one that works better and strips more space at the beginning.
	  (add-to-list 'compilation-error-regexp-alist-alist
				   '(go-test . ("^[[:space:]]*\\([_a-zA-Z./][_a-zA-Z0-9./]*\\):\\([0-9]+\\):.*$" 1 2)))
	  (add-to-list 'compilation-error-regexp-alist-alist
				   '(go-panic . ("^[[:space:]]*\\([_a-zA-Z./][_a-zA-Z0-9./]*\\):\\([0-9]+\\)[[:space:]].*$" 1 2)))
	  ;; override.
	  (add-to-list 'compilation-error-regexp-alist 'go-test t)
	  (add-to-list 'compilation-error-regexp-alist 'go-panic t)))

  :ensure t
  :mode "\\.go\\'")

(provide 'setup-golang-mode)

;;; setup-golang-mode.el ends here
