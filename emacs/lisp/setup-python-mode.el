;;; setup-python-mode -- customizations for Python programming language-info-alist

;;; Code:

(use-package emacs
  :ensure t
  :hook (python-mode . (lambda ()
						 (setq indent-tabs-mode nil
							   tab-width 4))))

(use-package python-black
  :after python
  ;; :demand t
  :ensure t
  :when (executable-find "black")
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package lsp-pyright
  :disabled
  :ensure t
  :hook (python-mode . (lambda ()
						 (require 'lsp-pyright)
						 (lsp))))		; or lsp-deferred

(use-package pyvenv
  :disabled
  :ensure t
  :hook (python-mode . pyvenv-mode)

  :config
  (when nil
	(pyvenv-mode t)
	(setq pyvenv-post-activation-hooks
		  (list (lambda ()
				  (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
	(setq pyvenv-post-deactivation-hooks
		  (list (lambda ()
				  (setq python-shell-interpreter "python3"))))))

(use-package elpy
  :disabled
  :ensure t
  :init
  (elpy-enable))

(provide 'setup-python-mode)

;;; setup-python-mode.el ends here
