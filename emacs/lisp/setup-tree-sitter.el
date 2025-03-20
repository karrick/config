;;; setup-tree-sitter -- setup tree-sitter package

;;; Commentary:

;;; Code:

;; tree-sitter is not yet configured properly.
(if (and (functionp 'treesit-available-p)
		 (treesit-available-p))
	(if t
		(message "tree-sitter is available but not configured")
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
		 (nil (global-set-key (kbd "C-x C-s") #'tree-sitter-ispell-run-buffer))))
	  (message "tree-sitter is configured"))
  (message "tree-sitter is *not* available")
  ;; Configure eglot to not remap various modes to run tree sitter wrapper.
  (dolist (mode '(
				  bash-mode
				  c++-mode
				  c-or-c++-mode
				  c-mode
				  cmake-mode
				  csharp-mode
				  css-mode
				  dockerfile-mode
				  elixir-mode
				  go-mod-mode
				  go-mode
				  heex-mode
				  html-mode
				  java-mode
				  js-mode
				  json-mode
				  lua-mode
				  php-mode
				  python-mode
				  ruby-mode
				  rust-mode
				  toml-mode
				  tsx-mode
				  typescript-mode
				  yaml-mode
				  ))
	(message "Remapping mode to disable tree-sitter: %s" mode)
	(add-to-list 'major-mode-remap-alist (list mode))))

(provide 'setup-tree-sitter)

;;; setup-tree-sitter.el ends here
