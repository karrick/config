;;; setup-eglot -- setup eglot package

;;; Commentary:

;;; Code:

(use-package eglot
  :after yasnippet

  :bind (:map eglot-mode-map
			  ("C-c h" . eldoc)
			  ("C-c o" . eglot-code-action-organize-imports)
			  ("C-c r" . eglot-rename))

  ;; See the eglot-server-programs variable, in addition to:
  ;; https://github.com/joaotavora/eglot#connecting-to-a-server
  ;;
  ;;   * bash-language-server & shellcheck
  ;;   * gopls
  ;;   * jedi-language-server (or pylsp, pyls, pyright) (for python)
  ;;   * rust-analyzer
  ;;   * typescript-language-server (also used for js modes)
  ;;   * yaml-language-server
  ;;   * zls (for zig)

  ;; Add hooks to each of the following modes to invoke #'eglot-ensure, which
  ;; ensures that eglot-mode is activated for each.
  ;;
  ;; NOTE: Some modes are commented out because some aspect of their operation
  ;; is not yet properly configured on all platforms.
  :hook
  ((
	;; bash-ts-mode sh-mode
	go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode
	;; js-mode js-ts-mode
	python-mode
	rust-ts-mode rust-mode
	;; tsx-ts-mode typescript-ts-mode typescript-mode
	;; yaml-ts-mode yaml-mode
	;; zig-mode
	) . eglot-ensure)

  :custom

  ;; Disable eglot use of flymake.
  ;; (eglot-stay-out-of '(flymake))

  ;; While the following configuration option is not necessary, it is set here
  ;; to non-nil in order to configure eglot to shut down servers when final
  ;; buffer closed for which each respective server was supporting.
  (eglot-autoshutdown t)

  ;; The following configuration lines were required for lsp-mode, and might
  ;; be needed for eglot, because they both do the same thing with the same
  ;; JSON protocol. However, I am leaving them disabled for the time being,
  ;; until I see that they are in-fact required for eglot operation.
  ;;
  ;; (gc-cons-threshold 1000000 "1 million")
  ;; (read-process-output-max (* 4 1024 1024) "4 MiB to handle larger payloads from LISP.")

  :ensure t)

(provide 'setup-eglot)

;;; setup-eglot.el ends here
