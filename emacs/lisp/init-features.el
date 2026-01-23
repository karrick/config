;;; init-features.el --- Features initialization  -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defvar my-missing-tools-warned (make-hash-table :test 'equal)
  "Tools we have already warned about this session.")

(defun my-warn-missing-tool (tool &optional context)
  "Warn once per session if TOOL is not found.  CONTEXT is a short string describing where it is used."
  (unless (executable-find tool)
	(unless (gethash tool my-missing-tools-warned)
	  (puthash tool t my-missing-tools-warned)
	  (display-warning 'external-tool
					   (format "Optional tool '%s' not found%s." tool
							   (if context (format " (%s)" context) ""))
					   :warning))))

(defun my-with-cli* (clis callback &optional context)
  "Find the first available executable in CLIS and call CALLBACK with its path.

CLIS is a list of program names strings.  The first one found in
`exec-path` is used.

CALLBACK is a function of one argument, which is the absolute
file-system path to the executable.  If none of the executables in CLIS
are found, emit a warning (once per session) using
`my-warn-missing-tool` and return nil.

CONTEXT is an optional short description included in the warning
message."
  (declare (indent 1))
  (let ((path
		 (seq-some (lambda (cli)
					 (executable-find cli))
				   clis)))
	(if (not path)
		(progn (my-warn-missing-tool (mapconcat #'identity clis ", ") context)
			   nil)
	  (funcall callback path))))

(cl-defmacro my-with-cli! ((var clis &optional context) &rest body)
  "If one of CLIS is found, bind its path to VAR and evaluate BODY.

CLIS is a list of executable names (strings).  The first one found in
`exec-path` is used.

VAR is bound to the absolute path of the executable.  If none of the
programs files are found, emit a warning (once per session) using
`my-warn-missing-tool` and skip BODY.

CONTEXT is an optional short description included in the warning
message."
  (declare (indent 1))
  `(my-with-cli* ',clis
	 (lambda (,var)
	   ,@body)
	 ,context))

(defmacro my-with-cli-let! (bindings &rest body)
  "Bind multiple CLI tools and run BODY if all are found.

BINDINGS is a list of (VAR CLIS CONTEXT), where:

VAR is bound to the absolute path of the executable.  If none of the
programs files are found, emit a warning (once per session) using
`my-warn-missing-tool` and skip BODY.

CLIS is a list of executable names (strings).  The first one found in
`exec-path` is used.

CONTEXT is an optional short description included in the warning
message.

Example:
	(my-with-cli-let!
	 ((goimports (\"goimports\") \"Go imports tool\")
	  (staticcheck (\"staticcheck\") \"Static analysis\"))
	 (message \"All tools available\"))"
  (declare (indent 1))
  (let ((forms body))
	(dolist (b (reverse bindings))
	  (setq forms
			`((my-with-cli!
				  ,b
				,@forms))))
	`(progn ,@forms)))

(init-time "COMMAND INTERFACE IN WINDOWS"
		   ;; ;; eat is Emulate A Terminal, which I am considering as a replacement for
		   ;; ;; xterm-color.
		   ;; (use-package eat
		   ;;   :custom
		   ;;   (eat-enable-auto-line-mode t)
		   ;;   :ensure t)

		   ;; prevent stuttering of command in inferior shell processes.
		   (defun my-comint-init ()
			 (setq comint-process-echoes t))
		   (add-hook 'comint-mode-hook 'my-comint-init)

		   (use-package async-shell-command-wrapper
			 :bind (("M-&" . ksm/async-shell-command)
					("ESC &" . ksm/async-shell-command)
					("<f7>" . ksm/async-shell-command)))

		   ;; xterm-color is superior to ansi-color
		   ;; (https://github.com/atomontage/xterm-color)
		   (use-package xterm-color
			 :config
			 ;; TERM is a Unix concept; avoid forcing it on Windows
			 (unless (eq system-type 'windows-nt)
			   (setenv "TERM" "xterm-256color"))

			 ;; compilation buffers
			 (setq compilation-environment '("TERM=xterm-256color"))
			 (defun my/advice-compilation-filter (f proc string)
			   "Convert ANSI color sequences appended to compilation buffer to Emacs text properties."
			   (funcall f proc (xterm-color-filter string)))
			 (advice-add 'compilation-filter :around #'my/advice-compilation-filter)

			 ;; eshell mode
			 (with-eval-after-load 'esh-mode
			   (add-hook 'eshell-before-prompt-hook
						 #'(lambda ()
							 (setq xterm-color-preserve-properties t)))
			   (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
			   (setq eshell-output-filter-functions
					 (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

			 ;; shell mode
			 (setq comint-output-filter-functions
				   (cons #'comint-osc-process-output
						 (remove 'ansi-color-process-output comint-output-filter-functions)))

			 (add-hook 'shell-mode-hook
					   #'(lambda ()
						   ;; (setenv "ETERM" (getenv-internal "TERM" initial-environment)) ; hack to make original term available to inferior shells

						   ;; Disable font-locking in this buffer to improve
						   ;; performance
						   (font-lock-mode 0)
						   ;; Prevent font-locking from being re-enabled in
						   ;; this buffer
						   (make-local-variable 'font-lock-function)
						   (setq font-lock-function (lambda (_) nil))
						   ;; Add xterm-color hook
						   (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

			 :ensure t)

		   (require 'ansi-color)
		   (require 'compile)

		   (defun ksm/colorize-buffer ()
			 "Convert ANSI color sequences in buffer to Emacs faces."
			 (interactive)
			 (let ((inhibit-read-only t))
			   (ansi-color-apply-on-region (point-min) (point-max))))

		   ;; (use-package eshell
		   ;;	 :config
		   ;;	 (add-hook 'eshell-preoutput-filter-functions
		   ;;			   'ansi-color-filter-apply))

		   (use-package comint
			 :config
			 (add-hook 'comint-preoutput-filter-functions
					   'ansi-color-filter-apply))

		   (defun compilation-apply-ansi-color ()
			 "Convert ANSI color sequences appended to compilation buffer to Emacs faces."
			 (let ((inhibit-read-only t))
			   ;; (message "compilation-apply-ansi-color: %d through %d" compilation-filter-start (point-max))
			   (ansi-color-apply-on-region compilation-filter-start (point-max))))

		   (setq compilation-filter-hook nil)

		   (add-hook 'compilation-filter-hook
					 'compilation-apply-ansi-color))

(init-time "MISCELLANEOUS"
		   (use-package browser-open)

		   ;; company -- complete anything
		   (use-package company
			 :after eglot
			 :ensure t
			 :hook (eglot-managed-mode prog-mode text-mode))

		   (use-package dictionary
			 ;; https://www.masteringemacs.org/article/wordsmithing-in-emacs
			 :bind ("M-#" . dictionary-lookup-definition)

			 :config
			 (add-to-list 'display-buffer-alist
						  '("^\\*Dictionary\\*" display-buffer-in-side-window
							(side . left)
							(window-width . 50)))

			 :custom (dictionary-server "dict.org"))

		   (use-package empty-string)	; ??? not sure why this is needed here

		   (use-package expand-region
			 :disabled
			 :bind (("M-=" . er/expand-region)
					("ESC =" . er/expand-region)
					("M--" . er/contract-region)
					("ESC -" . er/contract-region)))

		   (use-package find-file-dynamic
			 :after find-file-in-repository)

		   (use-package hrg)

		   (add-hook 'markdown-mode-hook #'visual-line-mode)

		   (use-package ksm-align)
		   (use-package ksm-list)

		   (use-package multiple-cursors
			 :disabled
			 :bind (("C-S-c C-S-c" . mc/edit-lines)
					("C-c C-S-c" . mc/edit-lines)
					("C->" . mc/mark-next-like-this)
					("C-<" . mc/mark-previous-like-this)
					("C-c C-<" . mc/mark-all-like-this)
					("C-c C->" . mc/mark-more-like-this-extended)))

		   ;; By default bind "C-x C-r" to rgrep, but when ripgrep command and
		   ;; deadgrep package are both available, rebind to the latter to use
		   ;; the former...
		   (let ((cmd (executable-find "rg")))
			 (if (not cmd)
				 (global-set-key (kbd "C-x C-r") #'rgrep)
			   (use-package deadgrep
				 :bind (("C-x C-r" . deadgrep))
				 :ensure t)))

		   (require 'setup-org-mode)

		   ;; NOTE: Seems to have been replaced by 'prog-fill-reindent-defun
		   ;; in Emacs 30.1.
		   (unless (fboundp 'prog-fill-reindent-defun)
			 (use-package unfill
			   :bind ("M-q" . unfill-toggle)
			   :ensure t))

		   (use-package sort-commas)

		   (use-package which-key
			 :ensure t
			 :config (which-key-mode)))

(init-time "PROGRAMMING"
		   ;; Tabs and indentation.
		   (defvaralias 'c-basic-offset 'tab-width)
		   (defvaralias 'cperl-indent-level 'tab-width)
		   (defvaralias 'perl-indent-level 'tab-width)
		   (defvaralias 'yaml-indent-level 'tab-width)

		   ;; (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
		   (add-hook 'prog-mode-hook #'hl-line-mode)

		   (defun aj-toggle-fold ()
			 "Toggle fold all lines larger than indentation on current line."
			 (interactive)
			 (let ((col 1))
			   (save-excursion
				 (back-to-indentation)
				 (setq col (+ 1 (current-column)))
				 (set-selective-display
				  (if selective-display nil (or col 1))))))
		   (global-set-key (kbd "C-x $") #'aj-toggle-fold)

		   (use-package clean-and-indent
			 :bind ("<f9>" . clean-and-indent))

		   (use-package copy-and-comment
			 :bind ("<f8>" . copy-and-comment))

		   (use-package make-shebang-executable)

		   (use-package nix-mode
			 :ensure t
			 :mode "\\.nix\\'")

		   (require 'setup-elisp-mode)
		   (require 'setup-flycheck)

		   (use-package setup-golang-mode
			 :after flycheck)

		   (require 'setup-javascript-mode)
		   (require 'setup-python-mode)

		   (use-package puppet-mode
			 :ensure t
			 :mode "\\.pp\\'")

		   (require 'setup-eglot)

		   (use-package flycheck-eglot
			 :after eglot flycheck
			 :config
			 (global-flycheck-eglot-mode 1))

		   (require 'setup-formatting)
		   (require 'setup-ruby-mode)
		   ;; (require 'setup-rust-mode)

		   (use-package setup-shell-script-mode
			 :after flycheck)

		   (require 'setup-tree-sitter)
		   (require 'setup-zig-mode)

		   (use-package yasnippet
			 :ensure t))

(init-time "SPELL CHECKER"
		   (let ((cmd (executable-find "hunspell")))
			 (if (null cmd)
				 (message "Cannot find spelling program: consider installing 'hunspell' and 'hunspell-en-us' packages.")
			   ;; When hunspell found, configure the spell-checking backend
			   ;; and frontend.

			   ;; Backend: ispell
			   (use-package ispell
				 ;; ispell is the backend interface between Emacs and several
				 ;; spell-check CLI programs.
				 :custom
				 (ispell-program-name "hunspell")
				 (ispell-dictionary "en_US")
				 ;; (ispell-extra-args '("-d" "en_US"))
				 :config
				 ;; (add-to-list 'spell-fu-ignore-major-modes 'json-mode)
				 ;; (add-to-list 'spell-fu-ignore-major-modes 'csv-mode)
				 ;; (add-to-list 'spell-fu-ignore-major-modes 'fundamental-mode)
				 (when (eq system-type 'windows-nt)
				   (setq ispell-hunspell-dict-paths-alist
						 '(("en_US" "C:/Program Files/Hunspell/dictionaries")))))

			   ;; TODO: Because spell-fu excels in some buffer modes but can
			   ;; do poorly in large buffers, would like to configure spell-fu
			   ;; and flycheck, both for buffers where they do well. However,
			   ;; it is imperative to not have both running in a particular
			   ;; buffer. While I have been happy with flycheck for a long
			   ;; time, I am giving spell-fu some testing.

			   ;; Frontend: flycheck
			   (use-package flyspell
				 ;; flyspell drives the ispell backend and updates the UI.
				 :hook ((prog-mode . flyspell-prog-mode)
						(text-mode . flyspell-mode)))

			   ;; ;; Frontend: spell-fu
			   ;; (use-package spell-fu
			   ;;	 :ensure t
			   ;;	 ;; :hook
			   ;;	 ;; ((prog-mode . spell-fu-mode)
			   ;;	 ;;  (text-mode . spell-fu-mode))
			   ;;	 :custom
			   ;;	 (spell-fu-idle-delay 0.25)   ;; or 0.0 for immediate
			   ;;	 (spell-fu-word-delimit-camel-case t)
			   ;;	 :init
			   ;;	 (spell-fu-global-mode))
			   )))

(init-time "WINDOW MANAGEMENT"
		   ;; WINDOW MANAGEMENT: Mimic tmux commands for sanity, but
		   ;; importantly, to keep ability to use emacs in a tmux frame, you
		   ;; need to use a different key prefix in emacs than tmux.
		   ;;
		   ;; REQUIREMENTS:
		   ;;
		   ;;   1. Fluidly change which window is current. Preferably hold
		   ;;   down one or more modifier keys and press cursor direction.
		   ;;
		   ;;   2. Fluidly swap current buffer with an adjacent buffer,
		   ;;   keeping the active buffer active. Preferably hold down one or
		   ;;   more modifier keys and press cursor direction.
		   ;;
		   ;;   3. Temporarily work on one buffer, then restore balanced
		   ;;   buffer configuration. (Bind #'maximize-window)

		   (use-package buffer-move
			 :ensure t
			 :bind (("C-x 4 i" . buf-move-up) ; swap buffer that has point with buffer above it
					("C-x 4 k" . buf-move-down) ; swap buffer that has point with buffer below it
					("C-x 4 j" . buf-move-left) ; swap buffer that has point with buffer on its left
					("C-x 4 l" . buf-move-right))) ; swap buffer that has point with buffer on its right

		   (use-package default-text-scale
			 :ensure t
			 :config (default-text-scale-mode))

		   (setq frame-title-format '("%b@"
									  (:eval (or (file-remote-p
												  default-directory 'host)
												 system-name))
									  " - Emacs"))

		   (use-package ibuffer
			 :ensure t
			 :bind (("C-x C-b" . #'ibuffer)))

		   (use-package ksm-window
			 :bind (("C-x w l" . ksm-window-config-load) ; restore window configuration from hash
					("C-x w s" . ksm-window-config-save) ; copy window configuration to hash
					("C-x w x" . ksm-window-config-swap) ; swap window configuration
					("C-x 0" . ksm/delete-window)		  ; extension to existing behavior
					("C-x 1" . ksm/delete-other-windows) ; extension to existing behavior
					;; ("C-x 2" . split-window-below) ; this is the default key binding
					;; ("C-x 3" . split-window-right) ; this is the default key binding
					("C-x -" . ksm-window-zoom-out) ; pop and restore window configuration from stack
					("C-x +" . ksm-window-zoom-in) ; push window configuration to stack and delete other windows
					("C-x C-p" . other-window-backward)))

		   (use-package ksm-window-scrolling
			 :bind (("M-N" . ksm/forward-line-scroll-up)
					("M-P" . ksm/previous-line-scroll-down)
					("M-n" . scroll-n-lines-forward)
					("M-p" . scroll-n-lines-backward)))

		   (use-package switch-window
			 :ensure t
			 :bind (("C-x q" . switch-window) ; like tmux C-z q, but only shows numbers to select when more than two windows
					;; ("C-x 0" . switch-window-then-delete)
					;; ("C-x 1" . switch-window-then-maximize) ; like tmux C-z 1, but without the ability to toggle
					;; ("C-x \"" . switch-window-then-split-below) ; like tmux C-z "
					;; ("C-x %" . switch-window-then-split-right) ; like tmux C-z %

					;; ("C-x 4 0" . switch-window-then-kill-buffer)
					;; ("C-x 4 d" . switch-window-then-dired)
					;; ("C-x 4 f" . switch-window-then-find-file)
					;; ("C-x 4 m" . switch-window-then-compose-mail)
					;; ("C-x 4 r" . switch-window-then-find-file-read-only)
					;; ("C-x 4 s" . switch-window-then-swap-buffer)

					;; ("C-x 4 C-f" . switch-window-then-find-file)
					;; ("C-x 4 C-o" . switch-window-then-display-buffer)
					))

		   (use-package windmove
			 :bind (("M-I" . windmove-up) ; move point to buffer above it
					("M-K" . windmove-down) ; move point to buffer below it
					("M-L" . windmove-right) ; move point to buffer on its right
					("M-J" . windmove-left))) ; move point to buffer on its left

		   (use-package window
			 :bind (("C-x =" . #'balance-windows)
					("C-x C-n" . other-window)))

		   (use-package zenburn-theme
			 :config
			 (load-theme 'zenburn t)
			 :ensure t))

(init-time "VCS"
		   (with-eval-after-load 'vc-hooks
			 (define-key vc-prefix-map "=" #'vc-ediff))

		   ;; fossil
		   (autoload 'vc-fossil-registered "vc-fossil")
		   (add-to-list 'vc-handled-backends 'Fossil)

		   ;; svn
		   (autoload 'svn-status "psvn"
			 "Examine the status of Subversion working copy in directory DIR.
If ARG is -, allow editing of the parameters. One could add -N to
run svn status non recursively to make it faster.  For every
other non nil ARG pass the -u argument to `svn status', which
asks svn to connect to the repository and check to see if there
are updates there.

If there is no .svn directory, examine if there is CVS and run
`cvs-examine'. Otherwise ask if to run `dired'."
			 t))

(init-time "KEY BINDINGS"
		   (global-set-key (kbd "C-x C-b") #'ibuffer)

		   (require 'compile)
		   (global-set-key (kbd "<f4>")  #'recompile)
		   (global-set-key (kbd "<f5>")  #'compile)

		   (global-set-key (kbd "<f6>")  #'delete-indentation)
		   (global-set-key (kbd "<f10>") #'revert-buffer)

		   ;; (define-key grep-mode-map (kbd "C-x C-q") #'wgrep-change-to-wgrep-mode)

		   ;; The following key-bindings (shown along with their default
		   ;; functions) below are disabled:
		   (global-unset-key (kbd "C-z")) ; suspend-frame
		   (global-unset-key (kbd "s-m")) ; iconify-frame
		   (global-unset-key (kbd "s-p")) ; ns-print-buffer
		   (global-unset-key (kbd "s-q")) ; save-buffers-kill-emacs
		   (global-unset-key (kbd "s-s")) ; save-buffer
		   (global-unset-key (kbd "s-t")) ; menu-set-font
		   (global-unset-key (kbd "s-z")) ; undo
		   )

(provide 'init-features)
;;; init-features.el ends here
