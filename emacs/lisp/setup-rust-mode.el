;;; setup-rust-mode --- customizations for the Rust programming language

;;; Commentary:

;;; Code:

;; configure emacs and environment for the Rust programming language

(require 'path)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(path-prepend (expand-file-name "~/.cargo/bin"))

(use-package rustic
  ;; Improvement upon rust-mode, with flycheck integration.
  ;;
  ;; https://github.com/brotzeit/rustic

  :config
  ;; (require 'rustic-rustfmt)

  (defun rk/rustic-mode-hook ()
	;; So that run C-c C-c C-r works without having to confirm, but don't try to
	;; save rust buffers that are not visiting a file. Once
	;; https://github.com/brotzeit/rustic/issues/253 has been resolved this
	;; should no longer be necessary.
	(when buffer-file-name
	  (setq-local buffer-save-without-query t))
	(add-hook 'before-save-hook #'lsp-format-buffer nil t))

  (let ((cmd (executable-find "rust-analyzer")))
	(if (null cmd)
		(message "Cannot find rust-analyzer")
	  ;; When rust-analyzer language server is found, install hooks for
	  ;; rust-mode to use it.

	  ;; "M-j" lsp-ui-imenu
	  ;; "M-?" lsp-find-reference
	  ;; "C-c C-c l" flycheck-list-erors
	  ;; "C-c C-c a" lsp-execute-code-action
	  ;; "C-c C-c r" lsp-rename
	  ;; "C-c C-c q" lsp-workspace-restart
	  ;; "C-c C-c Q" lsp-workspace-shutdown
	  ;; "C-c C-c s" lsp-rust-analyzer-status

	  (setq rustic-format-trigger 'on-save)
	  (add-hook 'rustic-mode-hook #'rk/rustic-mode-hook)))

  :ensure t)

(provide 'setup-rust-mode)
;;; setup-rust-mode.el ends here
