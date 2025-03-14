;;; setup-python-mode -- customizations for Python programming language-info-alist

;;; Code:

(use-package emacs
  :hook (python-mode . (lambda ()
						 (setq fill-column 80
							   indent-tabs-mode nil
							   tab-width 4))))

(use-package pyvenv
  :config
  (pyvenv-mode t)
  (setq pyvenv-post-activation-hooks
		(list (lambda ()
				(setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivation-hooks
		(list (lambda ()
				(setq python-shell-interpreter "python3"))))

  :custom
  (pyvenv-default-virtual-env-name "venv")

  :ensure t
  :hook (python-mode . pyvenv-mode))

(use-package ruff-format
  :ensure t
  :hook (python-mode . ruff-format-on-save-mode)
  :when (executable-find "ruff"))

(provide 'setup-python-mode)

;;; setup-python-mode.el ends here
