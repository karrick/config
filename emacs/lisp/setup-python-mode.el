;;; setup-python-mode -- customizations for Python programming language-info-alist

;;; Code:

(defun my-python-select-formatter ()
  "Select the appropriate formatter for this Python buffer."
  (cond
   ;; Prefer eglot
   ((eglot-managed-p)
	(when (bound-and-true-p ruff-format-on-save-mode)
	  (ruff-format-on-save-mode -1))
	(add-hook 'before-save-hook
			  #'my-eglot-format-and-organize-imports
			  nil t))

   ;; Fall back to ruff-format
   ((and (executable-find "ruff")
		 (fboundp 'ruff-format-on-save-mode))
	(ruff-format-on-save-mode 1))

   (t
	(my-warn-missing-tool "ruff" "Python formatting and imports"))))

(defun my-python-mode-setup ()
  "Common settings for Python buffers."
  (eglot-ensure)

  (setq-local fill-column 80
			  indent-tabs-mode nil
			  tab-width 4)

  (my-format-on-save!
	  (when (executable-find "ruff")
		#'ruff-format-buffer)
	"Python"))

;; flycheck should already be loaded, but repeated here.
(use-package flycheck
  :ensure t
  :defer t)

(use-package python
  :hook
  ((python-mode python-ts-mode) . my-python-mode-setup))

(with-eval-after-load 'eglot
  (my-with-cli!
	  (pylsp ("pylsp") "Python Language Server (Eglot)")
	;; Register pylsp for Go modes
	(add-to-list 'eglot-server-programs
				 `((python-mode python-ts-mode) . (,pylsp)))))

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
  :when (executable-find "ruff"))

(provide 'setup-python-mode)
;;; setup-python-mode.el ends here
