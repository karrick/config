;;; setup-tree-sitter -- setup tree-sitter package

;;; Commentary:

;;; Code:

;; tree-sitter is not yet configured properly.
(when (and (functionp 'treesit-available-p)
		   (treesit-available-p))
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

(provide 'setup-tree-sitter)

;;; setup-tree-sitter.el ends here
