;;; setup-golang-mode.el --- Go configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Eglot is used as the LSP client for Go due to superior performance and
;; operability compared to lsp-mode.  Eglot is included in modern Emacs.

;;; Code:

(require 'env-set-when-null)
(require 'ksm-system)

;;;; Environment setup

(env-set-when-null "GOBIN" (file-name-concat (getenv "XDG_DATA_HOME") (ksm/system-type) (ksm/system-arch) "bin") init-file-debug)
(env-set-when-null "GOCACHE" (file-name-concat (getenv "XDG_CACHE_HOME") "go-build") init-file-debug)
(env-set-when-null "GOMODCACHE" (file-name-concat (getenv "XDG_DATA_HOME") "go" "pkg" "mod") init-file-debug)
(env-set-when-null "GOTMPDIR" (file-name-concat (getenv "XDG_CACHE_HOME") "go-tmp") init-file-debug)

(let ((dir (getenv "GOTMPDIR")))
  (unless (file-directory-p dir)
	(make-directory dir 't)
	(message "Created GOTMPDIR: %s" dir)))

;;;; Go buffer setup

(defun my-go-mode-setup ()
  "Common settings for Go buffers."
  (eglot-ensure)

  (my-format-on-save! #'gofmt-before-save "Go")

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

;;;; Go mode

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"

  :config

  (my-with-cli!
	  (fmt ("goimports" "gofmt") "Go formatting commands")
	(setq gofmt-command fmt))

  (my-with-cli!
	  (_ ("gogetdoc") "Go documentation commands")
	(setq godoc-at-point-function #'godoc-gogetdoc))

  :hook
  ((go-mode go-ts-mode) . my-go-mode-setup)
  ((go-mode go-ts-mode) . flycheck-mode))

;;;; gopls (Go language server) configuration

(with-eval-after-load 'eglot
  (my-with-cli!
	  (gopls ("gopls") "Go language server (Eglot)")
	;; Register gopls for Go modes
	(add-to-list 'eglot-server-programs
				 `((go-mode go-ts-mode) . (,gopls)))

	;; gopls settings
	(add-to-list 'eglot-workspace-configuration
				 '(:gopls .
						  (:usePlaceholders t
											:staticcheck t
											:completeUnimported t)))))

(with-eval-after-load 'flycheck
  (my-with-cli!
	  (staticcheck ("staticcheck") "Flycheck Go checker (static analysis)")
	(setq flycheck-go-staticcheck-executable staticcheck)))

(provide 'setup-golang-mode)
;;; setup-golang-mode.el ends here
