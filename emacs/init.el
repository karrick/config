;;; package --- Summary: Emacs Initialization -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 00 Process Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make Elisp files in `~/.config/emacs/lisp' directory available.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10 Window Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ibuffer
  :bind (("C-x C-b" . #'ibuffer)))

(use-package buffer-move
  :bind (("C-x 4 i" . buf-move-up) ; swap buffer that has point with buffer above it
		 ("C-x 4 k" . buf-move-down) ; swap buffer that has point with buffer below it
		 ("C-x 4 j" . buf-move-left) ; swap buffer that has point with buffer on its left
		 ("C-x 4 l" . buf-move-right))) ; swap buffer that has point with buffer on its right

(use-package window
  :bind ("C-x =" . #'balance-windows))

(use-package windmove
  :bind (("M-I" . windmove-up) ; move point to buffer above it
		 ("M-K" . windmove-down) ; move point to buffer below it
		 ("M-L" . windmove-right) ; move point to buffer on its right
		 ("M-J" . windmove-left))) ; move point to buffer on its left

(use-package ksm-window
  ;; :load-path "lisp"
  :bind (("C-x j" . ksm/window-config-restore) ; jump to window configuration from hash
		 ("C-x p" . ksm/window-config-save) ; save window configuration to hash
		 ("C-x 0" . ksm/delete-window)	  ; extension to existing behavior
		 ("C-x 1" . ksm/delete-other-windows)	; extension to existing behavior
		 ;; ("C-x 2" . split-window-below) ; this is the default key binding
		 ;; ("C-x 3" . split-window-right) ; this is the default key binding
		 ("C-x -" . ksm/window-zoom-out) ; pop and restore window configuration from stack
		 ("C-x +" . ksm/window-zoom-in) ; push window configuration to stack and delete other windows
		 ))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 50 Sort
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; company -- complete anything
(use-package company
  :hook prog-mode)

;; On machines that do not have GNU version of `ls(1)` command, use a
;; substitute written in Elisp.
(use-package ls-lisp
  :unless (memq system-type '(gnu gnu/linux gnu/kfreebsd))
  :custom
  (ls-lisp-use-insert-directory-program nil "TODO"))

;; By default bind "C-x C-r" to rgrep, but when ripgrep command and deadgrep
;; package are both available, rebind to the latter to use the former...
(let ((cmd (executable-find "rg")))
  (if (not cmd)
	  (global-set-key (kbd "C-x C-r") #'rgrep)
	(use-package deadgrep
	  :bind (("C-x C-r" . deadgrep))
	  :config (setq deadgrep-executable cmd))))

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
 '(default-text-scale-amount 20)
 '(diff-switches "-u")
 '(dired-auto-revert-buffer t)
 '(dired-listing-switches "-AbFhl")
 '(ediff-diff-options "-w")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(fancy-splash-image "")
 '(fill-column 78)
 '(global-flycheck-mode t)
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
   '(buffer-move company deadgrep default-text-scale fic-mode find-file-in-repository flycheck gnu-elpa-keyring-update go-mode highlight-indent-guides jenkinsfile-mode js2-mode json-mode just-mode lsp-mode lsp-pyright lsp-ui markdown-mode nginx-mode nov projectile puppet-mode rust-mode rustic switch-window system-packages tree-sitter tree-sitter-indent tree-sitter-ispell tree-sitter-langs vc-fossil vterm which-key xterm-color yaml-mode zenburn-theme zig-mode))
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
