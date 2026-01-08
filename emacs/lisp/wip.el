;;; wip -- work in progress

;;; Commentary:

;;; Code:

(require 'ansi-color)
(require 'compile)

;; auto-complete source of Emoji
(use-package ac-emoji
  :defer t
  :ensure t
  :hook ((git-commit-mode markdown-mode) . ac-emoji-setup))

(defun colorize-buffer ()
  "Convert ANSI color sequences in buffer to Emacs faces."
  (interactive)
  (let ((inhibit-read-only t))
	(ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'eshell-preoutput-filter-functions
		  'ansi-color-filter-apply)

(add-hook 'comint-preoutput-filter-functions
		  'ansi-color-filter-apply)

(defun compilation-apply-ansi-color ()
  "Convert ANSI color sequences appended to compilation buffer to Emacs faces."
  (let ((inhibit-read-only t))
	;; (message "compilation-apply-ansi-color: %d through %d" compilation-filter-start (point-max))
	(ansi-color-apply-on-region compilation-filter-start (point-max))))

(setq compilation-filter-hook nil)

(add-hook 'compilation-filter-hook
		  'compilation-apply-ansi-color)

;; (defun ksm/json-format ()
;;   (interactive)
;;   (save-excursion
;;	(shell-command-on-region (region-beginning)
;;							 (region-end)
;;							 "fx ."
;;							 (buffer-name)
;;							 t)))

;; (when (executable-find "fx"))

(defun ksm/json-format ()
  (interactive)
  (save-excursion
	(shell-command-on-region (point-min)
							 (point-max)
							 "fx . | jq -S . | fx ."
							 (buffer-name)
							 t)))

(provide 'wip)
;;; wip.el ends here
