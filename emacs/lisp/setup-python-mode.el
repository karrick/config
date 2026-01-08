;;; setup-python-mode -- customizations for Python programming language-info-alist

;;; Code:

(defun my-python-mode-setup ()
  "Common settings for Python buffers."
  (setq-local fill-column 80
			  indent-tabs-mode nil
			  tab-width 4))

;; flycheck should already be loaded, but repeated here.
(use-package flycheck
  :ensure t
  :defer t)

(use-package python
  :after eglot flycheck

  :config
  (let ((cmd (executable-find "ruff")))
	(if (null cmd)
		(message "Cannot find 'ruff' program.")
	  (setq flycheck-python-ruff-executable cmd)))

  :hook
  ((python-mode python-ts-mode) . (eglot-ensure my-python-mode-setup)))

(use-package pyvenv
  :after python

  :config
  (pyvenv-mode t)

  ;; Keep python-shell in sync with active venv
  (add-hook 'pyvenv-post-activate-hooks
			(lambda ()
			  (setq python-shell-interpreter
					(expand-file-name "bin/python3" pyvenv-virtual-env))))

  (add-hook 'pyvenv-post-deactivate-hooks
			(lambda ()
			  (setq python-shell-interpreter "python3")))

  :custom
  (pyvenv-default-virtual-env-name "venv")

  :ensure t

  :hook
  ((python-mode python-ts-mode) . pyvenv-mode))

(use-package ruff-format
  :ensure t
  :hook ((python-mode python-ts-mode) . ruff-format-on-save-mode)
  :when (executable-find "ruff"))

(provide 'setup-python-mode)
;;; setup-python-mode.el ends here
