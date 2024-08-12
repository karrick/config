;;; setup-zig-mode --- customizations for the Zig programming language

;;; Commentary:

;;; Code:

;; Configure emacs and environment for the Go programming language.

(use-package zig-mode
  :after eglot
  :mode "\\.zig\\'"

  :config
  ;; This block sets up buffer scoped configuration and is invoked every time
  ;; a new zig-mode buffer is created.
  (add-hook 'zig-mode-hook
			#'(lambda ()
				(set (make-local-variable 'compile-command)
					 (if t "zig build test"
					   (concat "zig test " (buffer-file-name))))))

  :ensure t)

(provide 'setup-zig-mode)

;;; setup-zig-mode.el ends here
