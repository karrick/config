;;; package --- Summary: Emacs Initialization -*- mode: emacs-lisp -*-

;;; Commentary:

;; Needs to perform all init performed by ~/.profile, but not by ~/.shrc.

;;; Code:

;; load-path
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(require 'init-core)
(require 'init-features)

(if (locate-library "wip")
	(require 'wip)
  (message "no wip file found"))

(init-time "LOCAL"
		   (if (locate-library "local")
			   (require 'local)
			 (message "no local file found")))

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
 '(compilation-scroll-output 'first-error)
 '(confirm-kill-emacs 'yes-or-no-p)
 '(diff-switches "-u")
 '(dired-auto-revert-buffer t)
 '(dired-listing-switches
   (if (memq system-type '(gnu gnu/linux gnu/kfreebsd)) "-AbFhl" "-ahl"))
 '(ediff-diff-options "-w")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(fancy-splash-image "")
 '(fill-column 78)
 '(highlight-indent-guides-method 'character)
 '(inhibit-startup-screen t)
 '(line-number-mode t)
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
	 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
	 ("melpa-stable" . "https://stable.melpa.org/packages/")
	 ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(buffer-move company deadgrep default-text-scale dpkg-dev-el flycheck-eglot go-dlv go-mode markdown-mode nix-mode puppet-mode pyvenv ruff-format rustic spell-fu switch-window unfill which-key yaml-mode yasnippet zenburn-theme zig-mode))
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
