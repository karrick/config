;;; setup-python-mode -- customizations for Python programming language-info-alist

;;; Code:

(use-package elpy
  :ensure nil
  :disabled
  :init
  (elpy-enable))

(use-package emacs
  :hook (python-mode . (lambda ()
						 (setq indent-tabs-mode nil
							   tab-width 4))))

(use-package lsp-pyright
  :ensure nil
  :disabled
  :custom
  (lsp-pyright-python-executable-cmd "python3")
  :hook (python-mode . (lambda ()
						 (require 'lsp-pyright)
						 (lsp))))		; or lsp-deferred

(use-package python-black
  :after python
  :when (executable-find "black")
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package pyvenv
  :ensure t
  :hook (python-mode . pyvenv-mode)

  :config
  (pyvenv-mode t)
  (setq pyvenv-post-activation-hooks
		(list (lambda ()
				(setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivation-hooks
		(list (lambda ()
				(setq python-shell-interpreter "python3"))))
  :ensure t)

(provide 'setup-python-mode)

;;; setup-python-mode.el ends here
