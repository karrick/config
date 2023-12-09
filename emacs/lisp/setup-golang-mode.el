;;; setup-golang-mode -- customizations for Go programming language

;;; Commentary:

;; After several years of testing eglot and lsp-mode, eglot has had superior
;; performance and operability benefits over the competing lsp-mode package as
;; a Language Server Protocol client. Furthermore, recently, eglot has been
;; moved into Emacs distribution.

;;; Code:

(use-package go-mode
  :config

  ;; Prefer gogetdoc as it provides better documentation than godoc and godef.
  (when (executable-find "gogetdoc")
	(setq godoc-at-point-function #'godoc-gogetdoc))

  ;; Prefer goimports, but use gofmt when goimports not found.
  (setq gofmt-command (or (executable-find "goimports")
						  (executable-find "gofmt")
						  "gofmt"))

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
	(add-to-list 'compilation-error-regexp-alist 'go-panic t))

  :ensure t

  :hook (go-mode . (lambda ()
					 (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

  :mode "\\.go\\'"

  ;; This go-mode configuration has at least one customization that depends
  ;; upon the eglot package, which is now built into Emacs. I am not sure
  ;; whether :requires implies :after.
  :requires eglot)

(provide 'setup-golang-mode)

;;; setup-golang-mode.el ends here
