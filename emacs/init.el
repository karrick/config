;;; package --- Summary: Emacs Initialization -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 00 Process Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when init-file-debug
  (setq use-package-verbose t
		use-package-expand-minimally nil
		use-package-compute-statistics t
		debug-on-error t)
  (if (and (fboundp 'native-comp-available-p)
		   (native-comp-available-p))
	  (message "Native compilation is available")
	(message "Native complation is *not* available"))

  (if (and (functionp 'json-serialize) (json-serialize nil))
	  (message "Native JSON is available")
	(message "Native JSON is *not* available")))

;;; When "cert" file in user-emacs-directory, presumably placed there as a
;;; symbolic link to a host-specific yet non-standard system cert file, then
;;; configure gnutls to trust it, before we attempt to contact
;;; package-archives from which packages would be downloaded.
(if (not (gnutls-available-p))
	(message "GNU TLS is not available.")
  (with-eval-after-load 'gnutls
	(let ((cert (expand-file-name "cert" user-emacs-directory)))
	  (when (file-readable-p cert)
		(add-to-list 'gnutls-trustfiles cert)))))

;; When running in daemon mode, change process directory to user home
;; directory.
(if (daemonp) (cd (expand-file-name "~")))

;; Make Elisp files in `~/.config/emacs/lisp' directory available.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; To prioritize access latency over availability, ensure that highly
;; ephemeral cache data is stored on local machine rather than a home
;; directory that is potentially mounted over a network. However, do place all
;; cache files in a directory that makes it trivial to identify the owner and
;; optionally remove all cache data.
(unless (memq system-type '(darwin))
  (setenv "TMPDIR"
		  (let* ((logname (getenv "LOGNAME"))
				 (tmpdir (getenv "TMPDIR")))
			(cond
			 ((or (null tmpdir) (string-equal tmpdir ""))
			  (file-name-concat "/" "var" "tmp" logname))
			 ((string-suffix-p logname tmpdir)
			  tmpdir)
			 (t
			  (file-name-concat tmpdir logname))))))

;; https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
(use-package env-set-when-null
  :config
  (if init-file-debug
	  (progn
		(env-set-when-null-verbose "XDG_CACHE_HOME" (getenv "TMPDIR"))
		(env-set-when-null-verbose "XDG_CONFIG_HOME" (expand-file-name "~/.config"))
		(env-set-when-null-verbose "XDG_DATA_HOME" (expand-file-name "~/.local/share"))
		(env-set-when-null-verbose "XDG_STATE_HOME" (expand-file-name "~/.local/state"))
		(env-set-when-null-verbose "GOCACHE" (file-name-concat (getenv "XDG_CACHE_HOME") "go-build"))
		;; (env-set-when-null-verbose "GOBIN" (file-name-concat (getenv "XDG_DATA_HOME") os "bin"))
		(env-set-when-null-verbose "GOTMPDIR" (file-name-concat (getenv "XDG_CACHE_HOME") "go-tmp"))))
  (env-set-when-null "XDG_CACHE_HOME" (getenv "TMPDIR"))
  (env-set-when-null "XDG_CONFIG_HOME" (expand-file-name "~/.config"))
  (env-set-when-null "XDG_DATA_HOME" (expand-file-name "~/.local/share"))
  (env-set-when-null "XDG_STATE_HOME" (expand-file-name "~/.local/state"))
  (env-set-when-null "GOCACHE" (file-name-concat (getenv "XDG_CACHE_HOME") "go-build"))
  ;; (env-set-when-null "GOBIN" (file-name-concat (getenv "XDG_DATA_HOME") os "bin"))
  (env-set-when-null "GOTMPDIR" (file-name-concat (getenv "XDG_CACHE_HOME") "go-tmp")))

;; After XDG_DATA_HOME is set, can set PATH environment variable to any of the
;; directories I typically use, provided that they exist.
(use-package paths)

(use-package hrg
  :config
  (let* ((state (getenv "XDG_STATE_HOME"))
		 (history (file-name-concat state "history"))
		 (emacs (file-name-concat history "emacs")))
	(when (and state (file-directory-p history))
	  (setenv "HISTFILE" emacs))))

;; On machines that do not have GNU version of `ls(1)` command, use a
;; substitute written in Elisp.
(use-package ls-lisp
  :unless (memq system-type '(gnu gnu/linux gnu/kfreebsd))
  :custom
  (ls-lisp-use-insert-directory-program nil "TODO confusing documentation..."))

;; On Windows prefer using `plink.exe` program for TRAMP connections.
(when (and (eq system-type 'windows-nt) (executable-find "plink"))
  (setq tramp-default-method "plink"))

;; Elide `git(1)` paging capability for sub-processes:
(setenv "GIT_PAGER" "")

;; In lieu of paging files, dump them to a buffer using `cat(1)`:
(setenv "PAGER" (executable-find "cat"))

;; Make certain any child process knows to use `emacsclient(1)` as editor and
;; can route file editing requests to this process.
(let ((cmd (executable-find "emacsclient")))
  (when cmd
	(setenv "EDITOR" cmd)
	(setenv "VISUAL" cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10 EARLY INIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :config (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 20 Window Management
;;
;; WINDOW MANAGEMENT: Mimic tmux commands for sanity, but importantly, to keep
;; ability to use emacs in a tmux frame, you need to use a different key
;; prefix in emacs than tmux.
;;
;; REQUIREMENTS:
;;
;;   1. Fluidly change which window is current. Preferably hold down one or
;;   more modifier keys and press cursor direction.
;;
;;   2. Fluidly swap current buffer with an adjacent buffer, keeping the
;;   active buffer active. Preferably hold down one or more modifier keys and
;;   press cursor direction.
;;
;;   3. Temporarily work on one buffer, then restore balanced buffer
;;   configuration. (Bind #'maximize-window)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq frame-title-format '("%b@"
						   (:eval (or (file-remote-p
									   default-directory 'host)
									  system-name))
						   " - Emacs"))

(use-package buffer-move
  :bind (("C-x 4 i" . buf-move-up) ; swap buffer that has point with buffer above it
		 ("C-x 4 k" . buf-move-down) ; swap buffer that has point with buffer below it
		 ("C-x 4 j" . buf-move-left) ; swap buffer that has point with buffer on its left
		 ("C-x 4 l" . buf-move-right))) ; swap buffer that has point with buffer on its right

(use-package default-text-scale
  :config (default-text-scale-mode))

(use-package ibuffer
  :bind (("C-x C-b" . #'ibuffer)))

(use-package ksm-window
  ;; :load-path "lisp"
  :bind (("C-x j" . ksm/window-config-restore) ; jump to window configuration from hash
		 ("C-x p" . ksm/window-config-save) ; save window configuration to hash
		 ("C-x 0" . ksm/delete-window)		; extension to existing behavior
		 ("C-x 1" . ksm/delete-other-windows) ; extension to existing behavior
		 ;; ("C-x 2" . split-window-below) ; this is the default key binding
		 ;; ("C-x 3" . split-window-right) ; this is the default key binding
		 ("C-x -" . ksm/window-zoom-out) ; pop and restore window configuration from stack
		 ("C-x +" . ksm/window-zoom-in) ; push window configuration to stack and delete other windows
		 ("C-x C-p" . other-window-backward)))

;; (global-set-key "\C-x\C-n" 'other-window)

(use-package ksm-window-scrolling
  :bind (("M-N" . ksm/forward-line-scroll-up)
		 ("M-P" . ksm/previous-line-scroll-down)
		 ("M-n" . scroll-n-lines-forward)
		 ("M-p" . scroll-n-lines-backward)))

(use-package switch-window
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
  :bind ("C-x =" . #'balance-windows))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 30 MISCELLANEOUS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package align)

;; aspell
(let ((cmd (executable-find "aspell")))
  (if (null cmd)
	  (message "Cannot find spelling program: consider installing 'aspell' and 'en-aspell' packages.")
	(use-package ispell
	  :hook (prog-mode . flyspell-prog-mode)
	  :custom
	  (ispell-extra-args
	   '("--sug-mode=ultra" "--lang=en_US")
	   "ispell-extra-args contains actual parameters that will be passed to aspell."))))

(use-package async-shell-command-wrapper
  :bind (("M-&" . ksm/async-shell-command)
		 ("ESC &" . ksm/async-shell-command)
		 ("<f7>" . ksm/async-shell-command)))

(use-package browser-open)

(use-package clean-and-indent
  :bind ("<f9>" . clean-and-indent))

;; company -- complete anything
(use-package company
  :hook prog-mode)

(use-package copy-and-comment
  :bind ("<f8>" . copy-and-comment))

(use-package empty-string)

(use-package find-file-dynamic)

(use-package make-shebang-executable)

;; By default bind "C-x C-r" to rgrep, but when ripgrep command and deadgrep
;; package are both available, rebind to the latter to use the former...
(let ((cmd (executable-find "rg")))
  (if (not cmd)
	  (global-set-key (kbd "C-x C-r") #'rgrep)
	(use-package deadgrep
	  :bind (("C-x C-r" . deadgrep)))))

(use-package sort-commas)

(use-package unfill-paragraph
  :bind ("M-Q" . unfill-paragraph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 40 ORG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'setup-org-mode)
;; (require 'empty-string)
;; (require 'org)
;; (require 'org-mode-begin-src)

(use-package org
  :after empty-string

  :bind (("C-c a" . org-agenda)
		 ("C-c c" . org-capture))

  :config

  (defun org-mode-begin-src (language)
	"Insert an 'org-mode' source block using LANGUAGE."
	(interactive "sLanguage: ")
	(if (empty-string-p language)
		(insert (concat "#+BEGIN_SRC\n\n#+END_SRC\n"))
	  (insert (concat "#+BEGIN_SRC " language "\n\n#+END_SRC\n")))
	(previous-line 2))

  (define-key org-mode-map (kbd "C-c s") #'org-mode-begin-src)

  (require 'ol)
  (define-key org-mode-map (kbd "C-c l") #'org-store-link)

  (add-hook 'org-mode-hook #'(lambda ()
							   (local-set-key (kbd "C-c l") 'org-store-link)))
  :custom

  (org-clock-mode-line-today 'today)
  (org-indent-mode t)
  (org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "PR(p)" "|" "MERGED(m)" "DONE(d)" "CANCELLED(c)" "DELEGATED(g)")))

  (org-agenda-files '("~/gtd/inbox.org"
					  "~/gtd/projects.org"
					  "~/gtd/tickler.org")
					"TODO DOCUMENT")

  (org-capture-templates '(("t" "Todo [inbox]" entry
							(file+headline "~/gtd/inbox.org" "Inbox")
							"* TODO %i%?")
						   ;; ("p" "Project [projects]" entry
						   ;;  (file+headline "~/gtd/projects.org" "Projects")
						   ;;  "* TODO %i%?")
						   ("T" "Tickler" entry
							(file+headline "~/gtd/tickler.org" "Tickler")
							"* %i%? \n %U"))
						 "TODO DOCUMENT")

  (org-refile-targets '(("~/gtd/projects.org" :maxlevel . 3)
						("~/gtd/agendas.org" :level . 1)
						("~/gtd/inbox.org" :maxlevel . 2)
						("~/gtd/references.org" :level . 1)
						("~/gtd/someday.org" :level . 1)
						("~/gtd/tickler.org" :maxlevel . 2))
					  "TODO DOCUMENT")

  (org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
					 "TODO DOCUMENT"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 50 VCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 60 PROGRAMMING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tabs and indentation.

;; (defvaralias 'c-basic-offset 'tab-width)
;; (defvaralias
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

(use-package flycheck
  :defer t
  :config
  (global-flycheck-mode)
  (let ((cmd (executable-find "shellcheck")))
	(if (null cmd)
		(message "Cannot find 'shellcheck' program.")
	  (setq flycheck-sh-shellcheck-executable cmd)
	  (add-hook 'sh-mode-hook 'flycheck-mode))))

(use-package lsp-mode
  :after which-key
  :ensure t

  :init
  ;; Empirically discovered that lsp-keymap-prefix must be set before loading
  ;; lsp-mode.
  (setq lsp-keymap-prefix "C-c l")

  ;; :bind
  ;; ("C-x 4 M-." . xref-find-definitions-other-window)

  :custom
  (read-process-output-max (* 4 1024 1024) "4 MiB to handle larger payloads from LISP.")
  (gc-cons-threshold 1000000 "1 million")

  :config
  (when (featurep 'which-key)
	(lsp-enable-which-key-integration t)))

(add-hook 'markdown-mode-hook #'visual-line-mode)

(require 'setup-elisp-mode)

(use-package go-mode
  :after lsp-mode
  :ensure t

  :config

  ;; Use gogetdoc as it provides better documentation.
  (when (executable-find "gogetdoc")
	(setq godoc-at-point-function #'godoc-gogetdoc))

  (let ((cmd (executable-find "gopls")))
	(if cmd
		(progn
		  (lsp-register-custom-settings
		   '(("gopls.completeUnimported" t t)
			 ("gopls.staticcheck" t t)
			 ("gopls.usePlaceholders" t t)))

		  ;; When gopls language server is found, install hooks for go-mode to
		  ;; use it.
		  (when (featurep 'lsp-mode)
			(setq lsp-go-gopls-server-path cmd))

		  ;; Set up before-save hooks to format buffer and add modify
		  ;; imports. Ensure there is not another gofmt(1) or goimports(1)
		  ;; hook enabled.
		  (defun lsp-go-install-save-hooks ()
			(add-hook 'before-save-hook #'lsp-format-buffer t t)
			(add-hook 'before-save-hook #'lsp-organize-imports t t))

		  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
		  (add-hook 'go-mode-hook #'lsp))
	  (progn
		;; When cannot find gopls language server, configure a
		;; different go-mode-hook for graceful feature degredation.

		;; Prefer goimports, but when not found, use gofmt.
		(setq gofmt-command (or (executable-find "goimports")
								(executable-find "gofmt")))
		(add-hook 'go-mode-hook
				  #'(lambda ()
					  (add-hook 'before-save-hook #'gofmt-before-save nil t))))))

  (when nil
	;; Fix parsing of error and warning lines in compiler output.
	(setq compilation-error-regexp-alist-alist ; first remove the standard conf; it's not good.
		  (remove 'go-panic
				  (remove 'go-test compilation-error-regexp-alist-alist)))
	;; Make another one that works better and strips more space at the beginning.
	(add-to-list 'compilation-error-regexp-alist-alist
				 '(go-test . ("^[[:space:]]*\\([_a-zA-Z./][_a-zA-Z0-9./]*\\):\\([0-9]+\\):.*$" 1 2)))
	(add-to-list 'compilation-error-regexp-alist-alist
				 '(go-panic . ("^[[:space:]]*\\([_a-zA-Z./][_a-zA-Z0-9./]*\\):\\([0-9]+\\)[[:space:]].*$" 1 2)))
	;; override.
	(add-to-list 'compilation-error-regexp-alist 'go-test t)
	(add-to-list 'compilation-error-regexp-alist 'go-panic t))

  :mode ("\\.go\\'"))

(require 'setup-javascript-mode)
(require 'setup-python-mode)
(require 'setup-ruby-mode)
(require 'setup-rust-mode)
(require 'setup-zig-mode)

;; tree-sitter is not yet configured properly.
(when nil
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  (require 'tree-sitter-indent)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-indent-mode)
  (progn
	(require 'tree-sitter-ispell)
	(cond
	 (nil (global-set-key (kbd "C-x C-s") #'tree-sitter-ispell-run-at-point))
	 (nil (global-set-key (kbd "C-x C-s") #'tree-sitter-ispell-run-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 60 KEYS -- any additional key bindings not covered above
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-unset-key (kbd "C-z")) ; disable suspend-frame
(global-unset-key (kbd "s-p")) ; disable prompt to print a buffer
(global-unset-key (kbd "s-q")) ; disable abrupt Emacs exit
(global-unset-key (kbd "s-t")) ; disable ns-popup-font-panel
(global-unset-key (kbd "s-z")) ; disable minimize

(global-set-key (kbd "C-x C-b") #'ibuffer)

(require 'compile)
(global-set-key (kbd "<f4>")  #'recompile)
(global-set-key (kbd "<f5>")  #'compile)

(global-set-key (kbd "<f6>")  #'delete-indentation)
(global-set-key (kbd "<f10>") #'revert-buffer)

;; (define-key grep-mode-map (kbd "C-x C-q") #'wgrep-change-to-wgrep-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 70 Sort
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; xterm-color is superior to ansi-color
;;

;; compilation buffers
(defun my/advice-compilation-filter (f proc string)
  "Transform ANSI sequences in string to Emacs face."
  (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

;; shell mode
(setq comint-output-filter-functions
	  (remove 'ansi-color-process-output comint-output-filter-functions))
(add-hook 'shell-mode-hook
		  #'(lambda ()
			  ;; Disable font-locking in this buffer to improve
			  ;; performance
			  (font-lock-mode 0)
			  ;; Prevent font-locking from being re-enabled in
			  ;; this buffer
			  (make-local-variable 'font-lock-function)
			  (setq font-lock-function (lambda (_) nil))
			  ;; Add xterm-color hook
			  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

;; eshell mode
(with-eval-after-load 'esh-mode
  (add-hook 'eshell-before-prompt-hook
			#'(lambda ()
				(setq xterm-color-preserve-properties t)))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setenv "TERM" "xterm-256color"))

(use-package expand-region
  :disabled
  :bind (("M-=" . er/expand-region)
		 ("ESC =" . er/expand-region)
		 ("M--" . er/contract-region)
		 ("ESC -" . er/contract-region)))

(use-package multiple-cursors
  :disabled
  :bind (("C-S-c C-S-c" . mc/edit-lines)
		 ("C-c C-S-c" . mc/edit-lines)
		 ("C->" . mc/mark-next-like-this)
		 ("C-<" . mc/mark-previous-like-this)
		 ("C-c C-<" . mc/mark-all-like-this)
		 ("C-c C->" . mc/mark-more-like-this-extended)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 80 Wrap Up
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fset 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(put 'narrow-to-region 'disabled nil)

;; (desktop-save-mode 0)
;; (fido-mode 1)
;; (ido-mode 1)
;; (vertico-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 90 Localhost Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (locate-library "localhost")
	(require 'localhost)
  (message "no localhost file found"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 99 Custom Set Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(artist-text-renderer-function #'(lambda (someText) someText))
 '(auth-source-save-behavior nil)
 '(column-number-mode t)
 '(compilation-environment '("TERM=xterm-256color"))
 '(compilation-scroll-output 'first-error)
 '(confirm-kill-emacs 'yes-or-no-p)
 '(custom-enabled-themes '(zenburn))
 '(custom-safe-themes
   '("f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7" "5d966953e653a8583b3ad630d15f8935e9077f7fbfac456cb638eedad62f4480" "2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" "a3e99dbdaa138996bb0c9c806bc3c3c6b4fd61d6973b946d750b555af8b7555b" "fc48cc3bb3c90f7761adf65858921ba3aedba1b223755b5924398c666e78af8b" "70cfdd2e7beaf492d84dfd5f1955ca358afb0a279df6bd03240c2ce74a578e9e" "9040edb21d65cef8a4a4763944304c1a6655e85aabb6e164db6d5ba4fc494a04" "b77a00d5be78f21e46c80ce450e5821bdc4368abf4ffe2b77c5a66de1b648f10" "78e9a3e1c519656654044aeb25acb8bec02579508c145b6db158d2cfad87c44e" default))
 '(diff-switches "-u")
 '(dired-auto-revert-buffer t)
 '(dired-listing-switches "-AbFhl")
 '(ediff-diff-options "-w")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(fancy-splash-image "")
 '(fill-column 78)
 '(highlight-indent-guides-method 'character)
 '(inhibit-startup-screen t)
 '(line-number-mode t)
 '(lsp-enable-snippet nil)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(minibuffer-prompt-properties
   '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
 '(native-comp-async-report-warnings-errors 'silent)
 '(ns-function-modifier 'hyper)
 '(ns-use-srgb-colorspace t)
 '(package-archive-priorities '(("melpa-stable" . 2) ("melpa" . 1) ("gnu" . 0)))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
	 ("melpa-stable" . "https://stable.melpa.org/packages/")
	 ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(buffer-move company deadgrep default-text-scale fic-mode find-file-in-repository flycheck gnu-elpa-keyring-update highlight-indent-guides jenkinsfile-mode js2-mode json-mode just-mode lsp-pyright lsp-ui markdown-mode nginx-mode nov projectile puppet-mode rust-mode rustic switch-window system-packages tree-sitter tree-sitter-indent tree-sitter-ispell tree-sitter-langs vc-fossil vterm which-key xterm-color yaml-mode zenburn-theme zig-mode))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 5)
 '(sh-basic-offset 4)
 '(show-paren-mode t)
 '(show-paren-style 'expression)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(tsc-dyn-get-from '(:compilation))
 '(uniquify-buffer-name-style 'post-forward nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(visible-bell t)
 '(vterm-buffer-name-string "*vterm*|%s")
 '(vterm-copy-exclude-prompt nil)
 '(wgrep-auto-save-buffer t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit fixed-pitch :background "DarkOliveGreen" :foreground "gray75")))))

(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
