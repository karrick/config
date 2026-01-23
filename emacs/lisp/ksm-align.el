;;; ksm-align -- align non space columns in a region.

;;; Commentary:

;;; Code:

(defun ksm-align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))

(provide 'ksm-align)
;;; ksm-align.el ends here
