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
  (message "tree-sitter is *not* available"))

(provide 'setup-tree-sitter)

;;; setup-tree-sitter.el ends here
