;;; sudo -- Use TRAMP to `sudo' the current buffer

;;; Commentary:

;; Excerpt From
;; Mastering Emacs
;; Mickey Petersen
;; https://itunes.apple.com/WebObjects/MZStore.woa/wa/viewBook?id=0
;; This material may be protected by copyright.

;;; Code:

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
	(find-alternate-file
	 (concat "/sudo:root@localhost:"
			 buffer-file-name))))

(provide 'sudo)
;;; sudo.el ends here
