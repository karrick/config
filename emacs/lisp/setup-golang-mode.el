;;; setup-golang-mode -- customizations for Go programming language

;;; Commentary:

;; After several years of testing eglot and lsp-mode, eglot has had superior
;; performance and operability benefits over the competing lsp-mode package as
;; a Language Server Protocol client.  Furthermore, recently, eglot has been
;; moved into Emacs distribution.

;;; Code:

(progn
  (require 'ksm-system)
  (env-set-when-null "GOBIN" (file-name-concat (getenv "XDG_DATA_HOME") (ksm/system-type) (ksm/system-arch) "bin") init-file-debug)
  (env-set-when-null "GOCACHE" (file-name-concat (getenv "XDG_CACHE_HOME") "go-build") init-file-debug)
  (env-set-when-null "GOMODCACHE" (file-name-concat (getenv "XDG_DATA_HOME") "go" "pkg" "mod") init-file-debug)
  (env-set-when-null "GOTMPDIR" (file-name-concat (getenv "XDG_CACHE_HOME") "go-tmp") init-file-debug))

(let ((dir (getenv "GOTMPDIR")))
  (unless (file-directory-p dir)
	(make-directory dir 't)
	(message "Created GOTMPDIR: %s" dir)))

(use-package go-mode
  :after eglot
  :mode "\\.go\\'"

  :config

  ;; Use gogetdoc as it provides better documentation than godoc and godef.
  (when (executable-find "gogetdoc")
	(setq godoc-at-point-function #'godoc-gogetdoc))

  ;; Prefer goimports, but when not found, use gofmt.
  (setq gofmt-command (or (executable-find "goimports")
						  (executable-find "gofmt")))
  (add-hook 'go-mode-hook
			#'(lambda ()
				(if (functionp 'eglot-format-buffer)
					(add-hook 'before-save-hook #'eglot-format-buffer nil t)
				  (add-hook 'before-save-hook #'gofmt-before-save nil t))))

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

  :ensure t)

(provide 'setup-golang-mode)
;;; setup-golang-mode.el ends here
