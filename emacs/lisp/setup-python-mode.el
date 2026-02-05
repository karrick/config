;;; setup-python-mode -- customizations for Python programming language-info-alist

;;; Code:

(defun my-python-mode-setup ()
  "Common settings for Python buffers."
  (eglot-ensure)

  (setq-local fill-column 80
			  indent-tabs-mode nil
			  tab-width 4)

  (when nil
	(when (executable-find "ty")
	  (setq-default flycheck-disabled-checkers '(python-pylint python-flake8 python-mypy)) ; ???
	  (setq-local flycheck-checker 'python-ty)))

  (my-format-on-save!
	  (when (executable-find "ruff")
		#'ruff-format-buffer)
	"Python"))

(use-package python
  :hook
  ((python-mode python-ts-mode) . my-python-mode-setup))

;; flycheck should already be loaded, but repeated here.
(use-package flycheck
  :ensure t
  :defer t)

(when nil
  (with-eval-after-load 'flycheck
	(flycheck-define-checker python-ty
	  "A Python syntax and type checker using ty."

	  :command ("ty" "check"
				"--quiet"
				source-original)

	  :error-patterns
	  ((error   line-start
				(file-name) ":" line ":" column ": error: " (message)
				line-end)
	   (warning line-start
				(file-name) ":" line ":" column ": warning: " (message)
				line-end))

	  :modes (python-mode python-ts-mode))

	;; Prefer ty when available
	(when (executable-find "ty")
	  (add-to-list 'flycheck-checkers 'python-ty))))

(cond
 ((and nil (executable-find "ty"))
  (with-eval-after-load 'eglot
	(setf (alist-get '(python-mode python-ts-mode)
					 eglot-server-programs
					 nil nil #'equal)
		  '("ty" "server")))
  (setq-default python-check-command "ty check")
  (message "Python: using ty for eglot and syntax check"))

 ((executable-find "pylsp")
  (with-eval-after-load 'eglot
	(setf (alist-get '(python-mode python-ts-mode)
					 eglot-server-programs
					 nil nil #'equal)
		  '("pylsp")))
  (if (executable-find "ruff")
	  (progn
		(setq-default python-check-command "ruff check")
		(message "Python: using pylsp for eglot and ruff for syntax check"))
	(message "Python: using pylsp for eglot")))

 (t
  (if (executable-find "ruff")
	  (progn
		(setq-default python-check-command "ruff check")
		(message "Python: using ruff for syntax check"))
	(message "Python: no language server or syntax checker found"))))

;; (add-to-list 'eglot-server-programs
;;				`((python-mode python-ts-mode) . ("ty server")))
;; (setq python-check-command "ty check")))

;; (my-with-cli!
;;	(cmd ("ty") "Python Language Server (Eglot)")
;;   (with-eval-after-load 'eglot
;;	;; Register ty for Python modes
;;	(add-to-list 'eglot-server-programs
;;				 `((python-mode python-ts-mode) . (,cmd "server"))))
;;   ;; Tell python mode to use 'ty check'.
;;   (setq python-check-command (concat cmd " check")))

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
